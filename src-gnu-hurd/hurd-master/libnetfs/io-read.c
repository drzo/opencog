/*
   Copyright (C) 1995, 1996, 1997, 1999 Free Software Foundation, Inc.
   Written by Michael I. Bushnell, p/BSG.

   This file is part of the GNU Hurd.

   The GNU Hurd is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2, or (at
   your option) any later version.

   The GNU Hurd is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111, USA. */

#include "netfs.h"
#include "io_S.h"
#include <fcntl.h>
#include <sys/mman.h>

kern_return_t
netfs_S_io_read (struct protid *user,
		 data_t *data,
		 mach_msg_type_number_t *datalen,
		 off_t offset,
		 vm_size_t amount)
{
  error_t err;
  off_t start;
  struct node *node;
  int alloced = 0;
  size_t data_size = *datalen;

  if (!user)
    return EOPNOTSUPP;

  node = user->po->np;
  pthread_mutex_lock (&user->po->np->lock);

  if ((user->po->openstat & O_READ) == 0)
    {
      pthread_mutex_unlock (&node->lock);
      return EBADF;
    }

  if (amount > data_size)
    {
      alloced = 1;
      *data = mmap (0, amount, PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
    }
  data_size = amount;

  start = (offset == -1 ? user->po->filepointer : offset);

  if (start < 0)
    err = EINVAL;
  else if (S_ISLNK (node->nn_stat.st_mode))
    /* Read from a symlink.  */
    {
      off_t size = node->nn_stat.st_size;

      if (start + amount > size)
	amount = size - start;
      if (amount > size)
	amount = size;

      if (start >= size)
	{
	  data_size = 0;
	  err = 0;
	}
      else if (amount < size || start > 0)
	{
	  char *whole_link = alloca (size);
	  err = netfs_attempt_readlink (user->user, node, whole_link);
	  if (! err)
	    {
	      memcpy (*data, whole_link + start, amount);
	      data_size = amount;
	    }
	}
      else
	{
	  err = netfs_attempt_readlink (user->user, node, *data);
	  data_size = amount;
	}
    }
  else
    /* Read from a normal file.  */
    err = netfs_attempt_read (user->user, node, start, &data_size, *data);

  if (offset == -1 && !err)
    user->po->filepointer += data_size;

  pthread_mutex_unlock (&node->lock);

  if (err && alloced)
    munmap (*data, amount);

  if (!err && alloced && (round_page (data_size) < round_page (amount)))
    munmap (*data + round_page (data_size),
	    round_page (amount) - round_page (data_size));

  *datalen = data_size;
  return err;
}
