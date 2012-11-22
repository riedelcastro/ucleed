import sys

# allow this to be run more easily without $PYTHONPATH changes
sys.path.extend(('/u/mcclosky/res/ie/bionlp/reranker', '/home/mcclosky/local/python', '/home/mcclosky/rev/waterworks/'))

import BioNLP
from BioNLP import BioNLPEvents, bionlp_matching_events, get_unique_events
import PrecRec
from collections import defaultdict

event_classes = [
    'Acetylation DNA_methylation Glycosylation Hydroxylation Methylation Gene_expression Transcription Protein_catabolism Phosphorylation Localization Binding Process Ubiquitination Deacetylation DNA_demethylation Dehydroxylation Demethylation Dephosphorylation Deubiquitination'.split(),
    'Regulation Positive_regulation Negative_regulation Catalysis'.split(),
    ['Total'],
]

def evaluate(gold_directory_names, test_directory_names, doc_level=False, verbose=True, output_stream=None):
    output_stream = output_stream or sys.stdout

    gold_db = BioNLPEvents()
    for gold_directory_name in gold_directory_names.split(','):
        gold_db.read(gold_directory_name)

    test_db = BioNLPEvents()
    for gold_directory_name in gold_directory_names.split(','):
        test_db.read(gold_directory_name, just_a1_files=True)
    for test_directory_name in test_directory_names.split(','):
        test_db.read(test_directory_name, just_a2_files=True)

    total_gold = 0
    total_matched = 0
    total_proposed = 0

    gold_by_type = defaultdict(int)
    matched_by_type = defaultdict(int)
    proposed_by_type = defaultdict(int)

    for docid in test_db.docid_iterator():
        test_contents, _ = test_db.get_events_and_equiv_clusters(docid)
        gold_contents, gold_equiv_clusters = gold_db.get_events_and_equiv_clusters(docid)

        # need to reunique these using the actual gold clusters
        test_contents = get_unique_events(test_contents, gold_equiv_clusters)

        matched = bionlp_matching_events(gold_contents, gold_equiv_clusters, test_contents)

        for test_item in test_contents:
            item_type = test_item.event_type
            proposed_by_type[item_type] += 1
            proposed_by_type['Total'] += 1

        for gold_item in gold_contents:
            item_type = gold_item.event_type
            gold_by_type[item_type] += 1
            gold_by_type['Total'] += 1

            if gold_item in matched:
                matched_by_type[item_type] += 1
                matched_by_type['Total'] += 1

        if doc_level and verbose:
            print >>output_stream, 'DocID: %13s | G %4s M %4s P %4s |' % \
                (docid, len(gold_contents), len(matched), len(test_contents))

    def print_type(item_type):
        gold = gold_by_type[item_type]
        if gold == 0:
            return
        matched = matched_by_type[item_type]
        proposed = proposed_by_type[item_type]
        scores = PrecRec.precision_recall_f(matched, gold, proposed)
        formatted_scores = ['%.1f' % (score * 100) for score in scores]
        pieces = [item_type] + [gold, matched, proposed] + formatted_scores

        if verbose:
            print >>output_stream, '%-20s | G %4s M %4s P %4s | Prec %5s Rec %5s F %5s |' % tuple(pieces)
        return scores

    sep = '---------------------+----------------------+------------------------------+'
    if verbose:
        print >>output_stream, sep
    for event_class in event_classes:
        for item_type in event_class:
            scores = print_type(item_type)
        if verbose:
            print >>output_stream, sep

    return scores[-1] # i.e. the f-score

if __name__ == "__main__":
    import sys
    gold_directory_name, test_directory_name = sys.argv[-2:]
    print "Gold:", gold_directory_name
    print "Test:", test_directory_name
    evaluate(gold_directory_name, test_directory_name, doc_level=True)
