/*
 * nlp/fuzzy/Fuzzy.cc
 *
 * Copyright (C) 2015, 2016 OpenCog Foundation
 * All Rights Reserved
 *
 * Author: Leung Man Hin <https://github.com/leungmanhin>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/core/FindUtils.h>
#include <opencog/attentionbank/bank/AttentionBank.h>
#include <opencog/neighbors/GetPredicates.h>
#include <opencog/neighbors/Neighbors.h>
#include <opencog/nlp/types/atom_types.h>
#include <opencog/nlp/oc-types/atom_types.h>

#include "Fuzzy.h"

using namespace opencog::nlp;
using namespace opencog;

/**
 * The constructor.
 *
 * @param a   The AtomSpace that we are using
 * @param tt  The type of atoms we are looking for
 * @param ll  A list of atoms that we don't want them to exist in the results
 */
Fuzzy::Fuzzy(AtomSpace* a, Type tt, const HandleSeq& ll, bool af_only) :
    as(a),
    bank(&attentionbank(as)),
    rtn_type(tt),
    _af_only(af_only),
    excl_list(ll)
{
}

Fuzzy::Fuzzy(AtomSpace* a) :
    as(a),
    bank(&attentionbank(as)),
    _af_only(false)
{
}

Fuzzy::~Fuzzy()
{
}

/**
 * Examine a tree and get all the WordInstanceNodes and similar trees
 * associate with it
 *
 * @param h   The tree being examined
 * @param wi  A list of WordInstanceNodes
 * @param sl  A list of SimilarityLinks
 */
static void examine(const Handle& h, HandleSeq& wi, HandleSeq& sl)
{
    for (const Handle& lp : h->getIncomingSet())
    {
        if (h->is_node())
        {
            // TODO: Extend to find similar links as well
            if (lp->get_type() == SIMILARITY_LINK)
                sl.emplace_back(lp->get_handle());

            // Check if it's a link like this:
            //
            // ReferenceLink
            //   Concept "big@123"
            //   WordInstanceNode "big@123"
            //
            // that is generated by R2L via nlp-parse
            else if (lp->get_type() == REFERENCE_LINK)
            {
                const Handle& w = lp->getOutgoingAtom(1);

                if (w->get_type() == WORD_INSTANCE_NODE and
                    h->get_name() == w->get_name() and
                    std::find(wi.begin(), wi.end(), w) == wi.end())
                {
                    wi.emplace_back(w);
                }
            }
        }
    }

    if (h->is_link())
        for (const Handle& o : h->getOutgoingSet())
            examine(o, wi, sl);
}

/**
 * Get the WordNode associates with the WordInstanceNode via a LemmaLink,
 * assuming that it's in this form:
 *
 * LemmaLink
 *   WordInstanceNode "like@123"
 *   WordNode "like"
 *
 * @param h  The WordInstanceNode
 */
static Handle get_word(const Handle& h)
{
    return get_target_neighbors(h, LEMMA_LINK)[0];
}

/**
 * Compare the WordNode associates with the WordInstanceNode
 *
 * @param h1, h2  Handles of the nodes being compared
 */
static bool compare_word(const Handle& h1, const Handle& h2)
{
    return get_word(h1) < get_word(h2);
}

/**
 * Compare two trees and return a similarity score.
 *
 * @param h1, h2  The two trees that will be compared
 * @return        A similarity score between the two trees
 */
double Fuzzy::fuzzy_compare(const Handle& h1, const Handle& h2)
{
    start_search(h1);

    HandleSeq soln_word_insts;
    HandleSeq soln_simlks;
    examine(h2, soln_word_insts, soln_simlks);
    std::sort(soln_word_insts.begin(), soln_word_insts.end(), compare_word);
    std::sort(soln_simlks.begin(), soln_simlks.end());

    HandleSeq common_words;
    std::set_intersection(target_word_insts.begin(), target_word_insts.end(),
                          soln_word_insts.begin(), soln_word_insts.end(),
                          std::back_inserter(common_words), compare_word);

    HandleSeq common_simlks;
    std::set_intersection(target_simlks.begin(), target_simlks.end(),
                          soln_simlks.begin(), soln_simlks.end(),
                          std::back_inserter(common_simlks));

    auto rm_simlk = [&](const Handle& s)
    {
        const HandleSeq& os = s->getOutgoingSet();
        bool in_h1 = is_atom_in_tree(h1, os[0]);
        bool in_h2 = is_atom_in_tree(h2, os[0]);

        if (in_h1 and in_h2) return true;

        return false;
    };

    common_simlks.erase(std::remove_if(common_simlks.begin(),
        common_simlks.end(), rm_simlk), common_simlks.end());

    double score = 0;

    for (const Handle& c : common_words)
        score += get_score(c);

    for (const Handle& s : common_simlks)
        score += s->getTruthValue()->get_mean() * NODE_WEIGHT;

    score /= std::max(target_word_insts.size(), soln_word_insts.size());

    return score;
}

/**
 * A function intends to reflect how important a word is to a
 * document in a collection or corpus, which will be used in
 * the similarity estimation.
 *
 * @param words  A list of words (WordNode) of a sentence
 */
void Fuzzy::calculate_tfidf(const HandleSeq& word_insts)
{
    double min = std::numeric_limits<double>::max();
    double max = 0;

    // No. of words in the sentence
    int num_of_words = word_insts.size();

    // Total no. of sentences in the AtomSpace
    size_t num_of_sents = (size_t) as->get_num_atoms_of_type(SENTENCE_NODE);

    for (const Handle& wi : word_insts)
    {
        if (tfidf_weights.count(wi)) continue;

        Handle w = get_word(wi);

        auto word_match = [&](const Handle& h)
        {
            return w == get_word(h);
        };

        // No. of times this word exists in the sentence
        int word_cnt = std::count_if(word_insts.begin(), word_insts.end(), word_match);

        HandleSet hs;
        for (const Handle& l : get_source_neighbors(w, LEMMA_LINK))
        {
            for (const Handle& p : get_target_neighbors(l, WORD_INSTANCE_LINK))
            {
                const HandleSeq& sent_nodes = get_target_neighbors(p, PARSE_LINK);
                hs.insert(sent_nodes.begin(), sent_nodes.end());
            }
        }

        // No. of sentences that contain this word
        size_t num_sents_contains_it = hs.size();

        double tf = (double) word_cnt / num_of_words;
        double idf = log2((double) num_of_sents / num_sents_contains_it);
        double tfidf = tf * idf;

        tfidf_weights[wi] = tfidf;

        if (tfidf < min) min = tfidf;
        if (tfidf > max) max = tfidf;
    }

    // Normalize the values
    if (min != max)
        for (auto i = tfidf_weights.begin(); i != tfidf_weights.end(); i++)
            i->second = (i->second - min) / (max - min);
}

void Fuzzy::get_ling_rel(const HandleSeq& hs)
{
    for (const Handle& h : hs)
    {
        HandleSeq evals = get_predicates(h, DEFINED_LINGUISTIC_RELATIONSHIP_NODE);

        for (const Handle& el : evals)
        {
            // Extract the relation
            std::string ling_rel = (el->getOutgoingSet()[0])->get_name();

            // Assign weights accordingly, subject to change
            if (ling_rel.compare("_subj") == 0 or
                ling_rel.compare("_obj") == 0 or
                ling_rel.compare("_predadj") == 0)
            {
                ling_rel_weights[h] = LINGUISTIC_RELATION_WEIGHT;
                break;
            }
        }
    }
}

/**
 * Override the start_search() method, to get words instead of nodes.
 *
 * @param np  A NodePtr pointing to a node in the pattern
 */
void Fuzzy::start_search(const Handle& trg)
{
    target = trg;
    examine(target, target_word_insts, target_simlks);
    calculate_tfidf(target_word_insts);
    get_ling_rel(target_word_insts);
    std::sort(target_word_insts.begin(), target_word_insts.end(), compare_word);
    std::sort(target_simlks.begin(), target_simlks.end());
}

/**
 * Determine whether or not to accept a node as a starter node for fuzzy
 * matching. It accepts nodes that are not instances, as they are unlikely
 * to lead us to solutions that we want. They should also be either
 * ConceptNodes or PredicateNodes, as these type of atoms are likely to be
 * some actual words in sentences.
 *
 * @param hp  The target (input)
 * @return    True if an atom is accepted, false otherwise
 */
bool Fuzzy::accept_starter(const Handle& hp)
{
    if (hp->is_link())
        return false;

    Type t = hp->get_type();

    return (t == CONCEPT_NODE or t == PREDICATE_NODE) and
           hp->get_name().find("@") == std::string::npos;
}

/**
 * Determine whether or not to accept a potential solution found by the fuzzy
 * matcher. The potential solution has to be of the same type as the rtn_type,
 * and does not contain any unwanted atoms listed in the excl_list. The score
 * currently depends on the number of common words they both share (words
 * connects together by a SimilarityLink will also be considered as "common"
 * to a certain extent), the "rareness" of the words to all others existing
 * in the AtomSpace, and the linguistic relations of the words. The accepted
 * solutions will be stored in the solns vector.
 *
 * @param soln  The potential solution found
 * @return      True if the potential solution is accepted, false otherwise
 */
bool Fuzzy::try_match(const Handle& soln)
{
    if (_af_only and !bank->atom_is_in_AF(soln))
        return false;

    if (target == soln)
        return false;

    // Keep exploring if this is not the type of atom that we want,
    // until it reaches its root
    if (soln->get_type() != rtn_type)
        return true;

    // Reject if we have seen the exact same one before
    if (std::find(solns_seen.begin(), solns_seen.end(), soln) != solns_seen.end())
        return false;

    solns_seen.insert(soln);

    // Reject if it contains any unwanted atoms
    for (const Handle& excl : excl_list)
        if (is_atom_in_tree(soln, excl))
            return false;

    HandleSeq soln_word_insts;
    HandleSeq soln_simlks;
    examine(soln, soln_word_insts, soln_simlks);
    std::sort(soln_word_insts.begin(), soln_word_insts.end(), compare_word);
    std::sort(soln_simlks.begin(), soln_simlks.end());

    // Get the common words
    HandleSeq common_words;
    std::set_intersection(target_word_insts.begin(), target_word_insts.end(),
                          soln_word_insts.begin(), soln_word_insts.end(),
                          std::back_inserter(common_words), compare_word);

    // Reject if it's identical to the input
    if (common_words.size() == target_word_insts.size() and
        common_words.size() == soln_word_insts.size())
        return false;

    // Check if the soln has any atoms that are similar to the pattern
    HandleSeq common_simlks;
    std::set_intersection(target_simlks.begin(), target_simlks.end(),
                          soln_simlks.begin(), soln_simlks.end(),
                          std::back_inserter(common_simlks));

    auto rm_simlk = [&](const Handle& s)
    {
        const HandleSeq& os = s->getOutgoingSet();
        bool in_target = is_atom_in_tree(target, os[0]);
        bool in_soln = is_atom_in_tree(soln, os[0]);

        if (in_target and in_soln) return true;

        return false;
    };

    // Make sure one of the atoms in the SimilarityLink is from target,
    // and the other one is from soln
    common_simlks.erase(std::remove_if(common_simlks.begin(),
        common_simlks.end(), rm_simlk), common_simlks.end());

    // Initial value
    double score = 0;

    for (const Handle& c : common_words)
        score += get_score(c);

    for (const Handle& s : common_simlks)
        score += s->getTruthValue()->get_mean() * NODE_WEIGHT;

    score /= std::max(target_word_insts.size(), soln_word_insts.size());

    // Accept and store the solution
    if (score > 0)
        solns.push_back(std::make_pair(soln, score));

    return true;
}

/**
 * Get the score of a node that exists in both the target and potential solution
 *
 * @param h  The node for getting the score, should be a WordInstanceNode
 * @return   The score of the node
 */
double Fuzzy::get_score(const Handle& h)
{
    if (scores.find(h) != scores.end())
        return scores.at(h);

    double score = NODE_WEIGHT;
    score += tfidf_weights[h] * RARENESS_WEIGHT;
    score += ling_rel_weights[h];

    scores[h] = score;

    return score;
}

/**
 * Get method for getting the solutions sorted in descending order
 * of similarity.
 *
 * @return  A vector of solutions
 */
RankedHandleSeq Fuzzy::finished_search(void)
{
    // Sort the solutions by their similarity scores
    std::sort(solns.begin(), solns.end(),
        [] (std::pair<Handle, double> s1, std::pair<Handle, double> s2) {
            return s1.second > s2.second;
    });

    return solns;
}
