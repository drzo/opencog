/* This file was generated automatically from vm-engine.c; do not
   edit.  See the source file for copyright information.  */

#define FOR_EACH_VM_OPERATION(M) \
    M (0, halt, "halt", OP1 (X32)) \
    M (1, call, "call", OP2 (X8_F24, X8_C24)) \
    M (2, call_label, "call-label", OP3 (X8_F24, X8_C24, L32)) \
    M (3, tail_call, "tail-call", OP1 (X8_C24)) \
    M (4, tail_call_label, "tail-call-label", OP2 (X8_C24, L32)) \
    M (5, tail_call_shuffle, "tail-call/shuffle", OP1 (X8_F24)) \
    M (6, receive, "receive", OP2 (X8_F12_F12, X8_C24) | OP_DST) \
    M (7, receive_values, "receive-values", OP2 (X8_F24, B1_X7_C24)) \
    M (8, unused_8, NULL, NOP) \
    M (9, return_values, "return-values", OP1 (X8_C24)) \
    M (10, subr_call, "subr-call", OP1 (X32)) \
    M (11, foreign_call, "foreign-call", OP1 (X8_C12_C12)) \
    M (12, continuation_call, "continuation-call", OP1 (X8_C24)) \
    M (13, compose_continuation, "compose-continuation", OP1 (X8_C24)) \
    M (14, tail_apply, "tail-apply", OP1 (X32)) \
    M (15, call_cc, "call/cc", OP1 (X32)) \
    M (16, abort, "abort", OP1 (X32)) \
    M (17, builtin_ref, "builtin-ref", OP1 (X8_S12_C12) | OP_DST) \
    M (18, br_if_nargs_ne, "br-if-nargs-ne", OP2 (X8_C24, X8_L24)) \
    M (19, br_if_nargs_lt, "br-if-nargs-lt", OP2 (X8_C24, X8_L24)) \
    M (20, br_if_nargs_gt, "br-if-nargs-gt", OP2 (X8_C24, X8_L24)) \
    M (21, assert_nargs_ee, "assert-nargs-ee", OP1 (X8_C24)) \
    M (22, assert_nargs_ge, "assert-nargs-ge", OP1 (X8_C24)) \
    M (23, assert_nargs_le, "assert-nargs-le", OP1 (X8_C24)) \
    M (24, alloc_frame, "alloc-frame", OP1 (X8_C24)) \
    M (25, reset_frame, "reset-frame", OP1 (X8_C24)) \
    M (26, push, "push", OP1 (X8_S24)) \
    M (27, pop, "pop", OP1 (X8_S24) | OP_DST) \
    M (28, drop, "drop", OP1 (X8_C24)) \
    M (29, assert_nargs_ee_locals, "assert-nargs-ee/locals", OP1 (X8_C12_C12)) \
    M (30, br_if_npos_gt, "br-if-npos-gt", OP3 (X8_C24, X8_C24, X8_L24)) \
    M (31, bind_kwargs, "bind-kwargs", OP4 (X8_C24, C8_C24, X8_C24, N32)) \
    M (32, bind_rest, "bind-rest", OP1 (X8_F24) | OP_DST) \
    M (33, br, "br", OP1 (X8_L24)) \
    M (34, br_if_true, "br-if-true", OP2 (X8_S24, B1_X7_L24)) \
    M (35, br_if_null, "br-if-null", OP2 (X8_S24, B1_X7_L24)) \
    M (36, br_if_nil, "br-if-nil", OP2 (X8_S24, B1_X7_L24)) \
    M (37, br_if_pair, "br-if-pair", OP2 (X8_S24, B1_X7_L24)) \
    M (38, br_if_struct, "br-if-struct", OP2 (X8_S24, B1_X7_L24)) \
    M (39, br_if_char, "br-if-char", OP2 (X8_S24, B1_X7_L24)) \
    M (40, br_if_tc7, "br-if-tc7", OP2 (X8_S24, B1_C7_L24)) \
    M (41, br_if_eq, "br-if-eq", OP3 (X8_S24, X8_S24, B1_X7_L24)) \
    M (42, br_if_eqv, "br-if-eqv", OP3 (X8_S24, X8_S24, B1_X7_L24)) \
    M (43, unused_43, NULL, NOP) \
    M (44, br_if_logtest, "br-if-logtest", OP3 (X8_S24, X8_S24, B1_X7_L24)) \
    M (45, br_if_ee, "br-if-=", OP3 (X8_S24, X8_S24, B1_X7_L24)) \
    M (46, br_if_lt, "br-if-<", OP3 (X8_S24, X8_S24, B1_X7_L24)) \
    M (47, br_if_le, "br-if-<=", OP3 (X8_S24, X8_S24, B1_X7_L24)) \
    M (48, mov, "mov", OP1 (X8_S12_S12) | OP_DST) \
    M (49, long_mov, "long-mov", OP2 (X8_S24, X8_S24) | OP_DST) \
    M (50, long_fmov, "long-fmov", OP2 (X8_F24, X8_F24) | OP_DST) \
    M (51, box, "box", OP1 (X8_S12_S12) | OP_DST) \
    M (52, box_ref, "box-ref", OP1 (X8_S12_S12) | OP_DST) \
    M (53, box_set, "box-set!", OP1 (X8_S12_S12)) \
    M (54, make_closure, "make-closure", OP3 (X8_S24, L32, X8_C24) | OP_DST) \
    M (55, free_ref, "free-ref", OP2 (X8_S12_S12, X8_C24) | OP_DST) \
    M (56, free_set, "free-set!", OP2 (X8_S12_S12, X8_C24)) \
    M (57, make_short_immediate, "make-short-immediate", OP1 (X8_S8_I16) | OP_DST) \
    M (58, make_long_immediate, "make-long-immediate", OP2 (X8_S24, I32) | OP_DST) \
    M (59, make_long_long_immediate, "make-long-long-immediate", OP3 (X8_S24, A32, B32) | OP_DST) \
    M (60, make_non_immediate, "make-non-immediate", OP2 (X8_S24, N32) | OP_DST) \
    M (61, static_ref, "static-ref", OP2 (X8_S24, R32) | OP_DST) \
    M (62, static_set, "static-set!", OP2 (X8_S24, LO32)) \
    M (63, static_patch, "static-patch!", OP3 (X32, LO32, L32)) \
    M (64, current_module, "current-module", OP1 (X8_S24) | OP_DST) \
    M (65, resolve, "resolve", OP2 (X8_S24, B1_X7_S24) | OP_DST) \
    M (66, define, "define!", OP1 (X8_S12_S12) | OP_DST) \
    M (67, toplevel_box, "toplevel-box", OP5 (X8_S24, R32, R32, N32, B1_X31) | OP_DST) \
    M (68, module_box, "module-box", OP5 (X8_S24, R32, N32, N32, B1_X31) | OP_DST) \
    M (69, prompt, "prompt", OP3 (X8_S24, B1_X7_F24, X8_L24)) \
    M (70, wind, "wind", OP1 (X8_S12_S12)) \
    M (71, unwind, "unwind", OP1 (X32)) \
    M (72, push_fluid, "push-fluid", OP1 (X8_S12_S12)) \
    M (73, pop_fluid, "pop-fluid", OP1 (X32)) \
    M (74, fluid_ref, "fluid-ref", OP1 (X8_S12_S12) | OP_DST) \
    M (75, fluid_set, "fluid-set!", OP1 (X8_S12_S12)) \
    M (76, string_length, "string-length", OP1 (X8_S12_S12) | OP_DST) \
    M (77, string_ref, "string-ref", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (78, string_to_number, "string->number", OP1 (X8_S12_S12) | OP_DST) \
    M (79, string_to_symbol, "string->symbol", OP1 (X8_S12_S12) | OP_DST) \
    M (80, symbol_to_keyword, "symbol->keyword", OP1 (X8_S12_S12) | OP_DST) \
    M (81, cons, "cons", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (82, car, "car", OP1 (X8_S12_S12) | OP_DST) \
    M (83, cdr, "cdr", OP1 (X8_S12_S12) | OP_DST) \
    M (84, set_car, "set-car!", OP1 (X8_S12_S12)) \
    M (85, set_cdr, "set-cdr!", OP1 (X8_S12_S12)) \
    M (86, add, "add", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (87, add_immediate, "add/immediate", OP1 (X8_S8_S8_C8) | OP_DST) \
    M (88, sub, "sub", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (89, sub_immediate, "sub/immediate", OP1 (X8_S8_S8_C8) | OP_DST) \
    M (90, mul, "mul", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (91, div, "div", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (92, quo, "quo", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (93, rem, "rem", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (94, mod, "mod", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (95, ash, "ash", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (96, logand, "logand", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (97, logior, "logior", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (98, logxor, "logxor", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (99, make_vector, "make-vector", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (100, make_vector_immediate, "make-vector/immediate", OP1 (X8_S8_C8_S8) | OP_DST) \
    M (101, vector_length, "vector-length", OP1 (X8_S12_S12) | OP_DST) \
    M (102, vector_ref, "vector-ref", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (103, vector_ref_immediate, "vector-ref/immediate", OP1 (X8_S8_S8_C8) | OP_DST) \
    M (104, vector_set, "vector-set!", OP1 (X8_S8_S8_S8)) \
    M (105, vector_set_immediate, "vector-set!/immediate", OP1 (X8_S8_C8_S8)) \
    M (106, struct_vtable, "struct-vtable", OP1 (X8_S12_S12) | OP_DST) \
    M (107, allocate_struct, "allocate-struct", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (108, struct_ref, "struct-ref", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (109, struct_set, "struct-set!", OP1 (X8_S8_S8_S8)) \
    M (110, allocate_struct_immediate, "allocate-struct/immediate", OP1 (X8_S8_S8_C8) | OP_DST) \
    M (111, struct_ref_immediate, "struct-ref/immediate", OP1 (X8_S8_S8_C8) | OP_DST) \
    M (112, struct_set_immediate, "struct-set!/immediate", OP1 (X8_S8_C8_S8)) \
    M (113, class_of, "class-of", OP1 (X8_S12_S12) | OP_DST) \
    M (114, load_typed_array, "load-typed-array", OP5 (X8_S24, X8_S24, X8_S24, N32, C32) | OP_DST) \
    M (115, make_array, "make-array", OP4 (X8_S24, X8_S24, X8_S24, X8_S24) | OP_DST) \
    M (116, bv_u8_ref, "bv-u8-ref", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (117, bv_s8_ref, "bv-s8-ref", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (118, bv_u16_ref, "bv-u16-ref", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (119, bv_s16_ref, "bv-s16-ref", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (120, bv_u32_ref, "bv-u32-ref", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (121, bv_s32_ref, "bv-s32-ref", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (122, bv_u64_ref, "bv-u64-ref", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (123, bv_s64_ref, "bv-s64-ref", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (124, bv_f32_ref, "bv-f32-ref", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (125, bv_f64_ref, "bv-f64-ref", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (126, bv_u8_set, "bv-u8-set!", OP1 (X8_S8_S8_S8)) \
    M (127, bv_s8_set, "bv-s8-set!", OP1 (X8_S8_S8_S8)) \
    M (128, bv_u16_set, "bv-u16-set!", OP1 (X8_S8_S8_S8)) \
    M (129, bv_s16_set, "bv-s16-set!", OP1 (X8_S8_S8_S8)) \
    M (130, bv_u32_set, "bv-u32-set!", OP1 (X8_S8_S8_S8)) \
    M (131, bv_s32_set, "bv-s32-set!", OP1 (X8_S8_S8_S8)) \
    M (132, bv_u64_set, "bv-u64-set!", OP1 (X8_S8_S8_S8)) \
    M (133, bv_s64_set, "bv-s64-set!", OP1 (X8_S8_S8_S8)) \
    M (134, bv_f32_set, "bv-f32-set!", OP1 (X8_S8_S8_S8)) \
    M (135, bv_f64_set, "bv-f64-set!", OP1 (X8_S8_S8_S8)) \
    M (136, scm_to_f64, "scm->f64", OP1 (X8_S12_S12) | OP_DST) \
    M (137, f64_to_scm, "f64->scm", OP1 (X8_S12_S12) | OP_DST) \
    M (138, fadd, "fadd", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (139, fsub, "fsub", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (140, fmul, "fmul", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (141, fdiv, "fdiv", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (142, apply_non_program, "apply-non-program", OP1 (X32)) \
    M (143, scm_to_u64, "scm->u64", OP1 (X8_S12_S12) | OP_DST) \
    M (144, u64_to_scm, "u64->scm", OP1 (X8_S12_S12) | OP_DST) \
    M (145, bv_length, "bv-length", OP1 (X8_S12_S12) | OP_DST) \
    M (146, br_if_u64_ee, "br-if-u64-=", OP3 (X8_S24, X8_S24, B1_X7_L24)) \
    M (147, br_if_u64_lt, "br-if-u64-<", OP3 (X8_S24, X8_S24, B1_X7_L24)) \
    M (148, br_if_u64_le, "br-if-u64-<=", OP3 (X8_S24, X8_S24, B1_X7_L24)) \
    M (149, uadd, "uadd", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (150, usub, "usub", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (151, umul, "umul", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (152, uadd_immediate, "uadd/immediate", OP1 (X8_S8_S8_C8) | OP_DST) \
    M (153, usub_immediate, "usub/immediate", OP1 (X8_S8_S8_C8) | OP_DST) \
    M (154, umul_immediate, "umul/immediate", OP1 (X8_S8_S8_C8) | OP_DST) \
    M (155, load_f64, "load-f64", OP3 (X8_S24, AF32, BF32) | OP_DST) \
    M (156, load_u64, "load-u64", OP3 (X8_S24, AU32, BU32) | OP_DST) \
    M (157, scm_to_s64, "scm->s64", OP1 (X8_S12_S12) | OP_DST) \
    M (158, s64_to_scm, "s64->scm", OP1 (X8_S12_S12) | OP_DST) \
    M (159, load_s64, "load-s64", OP3 (X8_S24, AS32, BS32) | OP_DST) \
    M (160, current_thread, "current-thread", OP1 (X8_S24) | OP_DST) \
    M (161, logsub, "logsub", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (162, ulogand, "ulogand", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (163, ulogior, "ulogior", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (164, ulogsub, "ulogsub", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (165, ursh, "ursh", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (166, ulsh, "ulsh", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (167, scm_to_u64_truncate, "scm->u64/truncate", OP1 (X8_S12_S12) | OP_DST) \
    M (168, ursh_immediate, "ursh/immediate", OP1 (X8_S8_S8_C8) | OP_DST) \
    M (169, ulsh_immediate, "ulsh/immediate", OP1 (X8_S8_S8_C8) | OP_DST) \
    M (170, br_if_u64_ee_scm, "br-if-u64-=-scm", OP3 (X8_S24, X8_S24, B1_X7_L24)) \
    M (171, br_if_u64_lt_scm, "br-if-u64-<-scm", OP3 (X8_S24, X8_S24, B1_X7_L24)) \
    M (172, br_if_u64_le_scm, "br-if-u64-<=-scm", OP3 (X8_S24, X8_S24, B1_X7_L24)) \
    M (173, br_if_u64_gt_scm, "br-if-u64->-scm", OP3 (X8_S24, X8_S24, B1_X7_L24)) \
    M (174, br_if_u64_ge_scm, "br-if-u64->=-scm", OP3 (X8_S24, X8_S24, B1_X7_L24)) \
    M (175, integer_to_char, "integer->char", OP1 (X8_S12_S12) | OP_DST) \
    M (176, char_to_integer, "char->integer", OP1 (X8_S12_S12) | OP_DST) \
    M (177, ulogxor, "ulogxor", OP1 (X8_S8_S8_S8) | OP_DST) \
    M (178, make_atomic_box, "make-atomic-box", OP1 (X8_S12_S12) | OP_DST) \
    M (179, atomic_box_ref, "atomic-box-ref", OP1 (X8_S12_S12) | OP_DST) \
    M (180, atomic_box_set, "atomic-box-set!", OP1 (X8_S12_S12)) \
    M (181, atomic_box_swap, "atomic-box-swap!", OP2 (X8_S12_S12, X8_S24) | OP_DST) \
    M (182, atomic_box_compare_and_swap, "atomic-box-compare-and-swap!", OP3 (X8_S12_S12, X8_S24, X8_S24) | OP_DST) \
    M (183, handle_interrupts, "handle-interrupts", OP1 (X32)) \
    M (184, return_from_interrupt, "return-from-interrupt", OP1 (X32)) \
    M (185, push_dynamic_state, "push-dynamic-state", OP1 (X8_S24)) \
    M (186, pop_dynamic_state, "pop-dynamic-state", OP1 (X32)) \
    M (187, br_if_f64_ee, "br-if-f64-=", OP3 (X8_S24, X8_S24, B1_X7_L24)) \
    M (188, br_if_f64_lt, "br-if-f64-<", OP3 (X8_S24, X8_S24, B1_X7_L24)) \
    M (189, br_if_f64_le, "br-if-f64-<=", OP3 (X8_S24, X8_S24, B1_X7_L24)) \
    M (190, br_if_f64_gt, "br-if-f64->", OP3 (X8_S24, X8_S24, B1_X7_L24)) \
    M (191, br_if_f64_ge, "br-if-f64->=", OP3 (X8_S24, X8_S24, B1_X7_L24)) \
    M (192, string_set, "string-set!", OP1 (X8_S8_S8_S8)) \
    M (193, unused_193, NULL, NOP) \
    M (194, unused_194, NULL, NOP) \
    M (195, unused_195, NULL, NOP) \
    M (196, unused_196, NULL, NOP) \
    M (197, unused_197, NULL, NOP) \
    M (198, unused_198, NULL, NOP) \
    M (199, unused_199, NULL, NOP) \
    M (200, unused_200, NULL, NOP) \
    M (201, unused_201, NULL, NOP) \
    M (202, unused_202, NULL, NOP) \
    M (203, unused_203, NULL, NOP) \
    M (204, unused_204, NULL, NOP) \
    M (205, unused_205, NULL, NOP) \
    M (206, unused_206, NULL, NOP) \
    M (207, unused_207, NULL, NOP) \
    M (208, unused_208, NULL, NOP) \
    M (209, unused_209, NULL, NOP) \
    M (210, unused_210, NULL, NOP) \
    M (211, unused_211, NULL, NOP) \
    M (212, unused_212, NULL, NOP) \
    M (213, unused_213, NULL, NOP) \
    M (214, unused_214, NULL, NOP) \
    M (215, unused_215, NULL, NOP) \
    M (216, unused_216, NULL, NOP) \
    M (217, unused_217, NULL, NOP) \
    M (218, unused_218, NULL, NOP) \
    M (219, unused_219, NULL, NOP) \
    M (220, unused_220, NULL, NOP) \
    M (221, unused_221, NULL, NOP) \
    M (222, unused_222, NULL, NOP) \
    M (223, unused_223, NULL, NOP) \
    M (224, unused_224, NULL, NOP) \
    M (225, unused_225, NULL, NOP) \
    M (226, unused_226, NULL, NOP) \
    M (227, unused_227, NULL, NOP) \
    M (228, unused_228, NULL, NOP) \
    M (229, unused_229, NULL, NOP) \
    M (230, unused_230, NULL, NOP) \
    M (231, unused_231, NULL, NOP) \
    M (232, unused_232, NULL, NOP) \
    M (233, unused_233, NULL, NOP) \
    M (234, unused_234, NULL, NOP) \
    M (235, unused_235, NULL, NOP) \
    M (236, unused_236, NULL, NOP) \
    M (237, unused_237, NULL, NOP) \
    M (238, unused_238, NULL, NOP) \
    M (239, unused_239, NULL, NOP) \
    M (240, unused_240, NULL, NOP) \
    M (241, unused_241, NULL, NOP) \
    M (242, unused_242, NULL, NOP) \
    M (243, unused_243, NULL, NOP) \
    M (244, unused_244, NULL, NOP) \
    M (245, unused_245, NULL, NOP) \
    M (246, unused_246, NULL, NOP) \
    M (247, unused_247, NULL, NOP) \
    M (248, unused_248, NULL, NOP) \
    M (249, unused_249, NULL, NOP) \
    M (250, unused_250, NULL, NOP) \
    M (251, unused_251, NULL, NOP) \
    M (252, unused_252, NULL, NOP) \
    M (253, unused_253, NULL, NOP) \
    M (254, unused_254, NULL, NOP) \
    M (255, unused_255, NULL, NOP) \

