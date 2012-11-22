"""BioNLP corpus specific code.  Handles reading of gold events and
computing BioNLP approximate recursive f-scores."""
from path import path
from waterworks.Tools import initialize, generic_repr
from waterworks.Strings import multisplit
from iterextras import batch

debug_matching = False

# this magic is quite unfortunate but has to do with isinstance() not working properly for pickled objects
old_isinstance = isinstance
def relaxed_isinstance(obj, class_or_classes):
    if not old_isinstance(class_or_classes, (tuple, list)):
        class_or_classes = [class_or_classes]
    obj_classname = obj.__class__.__name__.split('.')[-1]
    for candidate_class in class_or_classes:
        candidate_classname = candidate_class.__name__.split('.')[-1]
        if obj_classname == candidate_classname:
            return True
    else:
        return False
isinstance = relaxed_isinstance

# XXX domain specific
def generalized_type(cpostag):
    if cpostag == 'Protein':
        return 'protein'
    elif cpostag.lower().endswith('regulation'):
        return 'complex'
    else:
        return 'simple'

def events_from_parse(sentence, parse):
    """Converts a DataSet.Parse object into a list of events."""
    sentence_pieces = sentence.metadata['SENT'].split()

    # these are precomputed now
    test_events = [obj for obj in parse.bionlp_events if isinstance(obj, Event)]

    if debug_matching:
        # print 'events_from_parse: docid =', sentence.metadata['DOC']
        # print 'graph:'
        # print 'graph', parse.graph
        # print 'bio graph', parse.bionlp_events_graph
        print 'test_events (preuniquing):'
        for i, event in enumerate(test_events):
            print ' %d\t%s' % (i, event.pretty_format())

    docid = sentence.metadata['DOC']
    gold_events, gold_equiv_clusters = get_events_db().get_events_and_equiv_clusters(docid)

    document_text_filename = get_events_db().pmid_to_text[docid]
    document_text = file(document_text_filename).read()
    text_picture = [' '] * len(document_text)
    for event in gold_events:
        term = event.event_term
        for index in range(term.start_offset, term.end_offset + 1):
            text_picture[index] = 'E'

    for gold_event in gold_events:
        gold_event.expand_token_offsets(document_text, text_picture)
    for cluster in gold_equiv_clusters.values():
        for item in cluster:
            item.expand_token_offsets(document_text, text_picture)

    test_events = get_unique_events(test_events, gold_equiv_clusters)

    if debug_matching:
        print 'test_events (postuniquing):'
        for i, event in enumerate(test_events):
            print ' %d\t%s' % (i, event.pretty_format())
        print 'gold_events:'
        for i, event in enumerate(gold_events):
            print ' %d\t%s' % (i, event.pretty_format())

    return test_events, gold_events, gold_equiv_clusters

def normalize_arg_type(arg):
    """Lowercases and removes trailing numbers."""
    assert '/' not in arg
    arg = arg.lower()
    while arg and arg[-1].isdigit():
        arg = arg[:-1]
    return arg

def bionlp_matching_events(gold_events, gold_equiv_clusters, test_events):
    """Returns the number of matching events.  This is the main function for
    doing BioNLP evaluations."""
    gold_events = list(gold_events)
    gold_events.sort()
    test_events = list(test_events)
    test_events.sort()
    matched_gold_events = set()
    matched_test_events = set()

    if debug_matching:
        print 'lengths', len(gold_events), len(gold_equiv_clusters), len(test_events)
        print 'gold_equiv_clusters', gold_equiv_clusters

    for test_event in test_events:
        if test_event in matched_test_events:
            continue

        for gold_event in gold_events:
            if gold_event in matched_gold_events:
                continue

            # print
            # print 'trying to match gold', gold_event.pretty_format(), gold_event.get_interval()
            # print '             vs test', test_event.pretty_format(), test_event.get_interval()
            if gold_event.matches(test_event, gold_equiv_clusters):
                # print 'matched gold', gold_events.index(gold_event), 'test', test_events.index(test_event), gold_event.event_type, test_event.event_type
                if debug_matching:
                    print 'matched gold', gold_event.pretty_format()
                    print 'test', test_event.pretty_format(), gold_event.event_type, test_event.event_type
                # raise 'z'
                matched_gold_events.add(gold_event)
                matched_test_events.add(test_event)
                # print 'matched lengths', len(matched_gold_events), len(matched_test_events)
                break

    # print 'bionlp_matching_events:', len(matched_gold_events)
    return matched_gold_events

def get_unique_events(events, gold_equiv_clusters):
    # print 'get_unique_events'
    # print 'gold_equiv_clusters', gold_equiv_clusters
    uniqued_events = set()
    events = sorted(list(events))
    for i, event1 in enumerate(events):
        matched = False
        for j, event2 in enumerate(events):
            if j <= i:
                continue
            # print 'i', i, 'j', j, event1.id, event2.id
            # print event1.id, event1.pretty_format()
            # print event2.id, event2.pretty_format()
            # print
            if event1.matches(event2, gold_equiv_clusters, approximate='never'):
                # print 'matched', event1.pretty_format()
                # print '   with', event2.pretty_format()
                # print
                matched = True
                break
        if not matched:
            uniqued_events.add(event1)
    return uniqued_events

_events_db = None
def get_events_db(basedir='/u/nlp/data/bioNLP/bioNLP09-09082010-ensemble-triggers'):
    global _events_db
    if _events_db is None:
        print 'Creating BioNLP events database... (really fixed BioNLP f-score metric!)'
        basedir = path(basedir)
        _events_db = BioNLPEvents()
        _events_db.read(basedir/'training')
        _events_db.read(basedir/'development')
        _events_db.read(basedir/'testing')
    return _events_db

class Equiv:
    def __init__(self, file_id, *terms):
        if file_id is not None:
            file_id = file_id.basename()
            try:
                file_id = int(file_id)
            except ValueError:
                pass
        # this is so we have some unique ID
        id = (file_id, terms)
        initialize(self, locals())
        self._not_in_repr = ['file_id', 'id']
    __repr__ = generic_repr
    def resolve(self, id_mapping):
        self.terms = [id_mapping[term] for term in self.terms]

class Term:
    def __init__(self, file_id, id, term_type, start_offset, end_offset, name, given):
        start_offset = int(start_offset)
        end_offset = int(end_offset)
        if file_id is not None:
            file_id = file_id.basename()
            try:
                file_id = int(file_id)
            except ValueError:
                pass
        initialize(self, locals())
        self._not_in_repr = ['file_id', 'id']
        self._already_expanded = False
    __repr__ = generic_repr
    def __hash__(self):
        return hash((self.file_id, self.id, self.term_type,
                     self.start_offset, self.end_offset, self.name))
    def get_interval(self):
        return (self.start_offset, self.end_offset)
    def resolve(self, id_mapping):
        """This is a no op since Term objects don't have any arguments to resolve."""
    def in_span(self, start_offset, end_offset):
        return start_offset <= self.start_offset <= self.end_offset <= end_offset
    def in_span_orig(self, start_offset, end_offset):
        """Tries to use original (unexpanded span) if possible."""
        try:
            our_start_offset = self.original_start_offset
            our_end_offset = self.original_end_offset
        except AttributeError:
            our_start_offset = self.start_offset
            our_end_offset = self.end_offset
        return start_offset <= our_start_offset <= our_end_offset <= end_offset
    def matches(self, other_term, gold_equiv_clusters, approximate=True):
        """Note that the approximate and gold_equiv_clusters args are ignored."""
        if not isinstance(other_term, Term):
            return False
        if self.term_type != other_term.term_type:
            return False
        if approximate:
            return other_term.in_span(self.start_offset, self.end_offset)
        else:
            return self.get_interval() == other_term.get_interval()
    def expand_token_offsets(self, document_text, text_picture):
        """To implement approximate scoring, this method expands the
        offsets of Terms by one word.  This should only be called on
        all terms in gold events to get the correct evaluation method."""
        # print 'expand_token_offsets already', self.pretty_format(), self._already_expanded
        if self._already_expanded:
            return
        else:
            self._already_expanded = True

            if self.given:
                # these don't get expanded
                return

        self.original_start_offset = self.start_offset
        self.original_end_offset = self.end_offset

        char_punc = set(''' .!?,'"''')
        def no_punc(char):
            return char not in char_punc

        ebeg = self.start_offset - 2
        while ebeg >= 0 and no_punc(document_text[ebeg]) and text_picture[ebeg] != 'E':
            ebeg -= 1
        ebeg += 1

        eend = self.end_offset + 2
        text_length = len(document_text)
        while eend < text_length and no_punc(document_text[eend - 1]) and text_picture[eend - 1] != 'E':
            eend += 1
        eend -= 1

        self.start_offset = ebeg
        self.end_offset = eend

        if debug_matching:
            print 'expand_token_offsets', self.pretty_format(), (self.original_start_offset, self.original_end_offset)
            print 'to', self.start_offset, self.end_offset

    def pretty_format(self, depth=0):
        return '%s %s (%s) [%d:%d]' % (self.id, self.name, self.term_type, self.start_offset, self.end_offset)

class Event:
    def __init__(self, file_id, id, event_type, event_term, args):
        if file_id is not None:
            file_id = file_id.basename()
            try:
                file_id = int(file_id)
            except ValueError:
                pass
        initialize(self, locals())
        self._not_in_repr = ['file_id', 'id']
    def __hash__(self):
        return hash((self.file_id, self.id, self.event_type, self.event_term, 
                     tuple(sorted(self.args))))
    def in_span_orig(self, start_offset, end_offset):
        return self.event_term.in_span_orig(start_offset, end_offset)
    def get_interval(self):
        args = sorted((relation_name, item.get_interval())
            for (relation_name, item) in self.args)
        return (self.event_term.get_interval(), args)
    def __cmp__(self, other_item):
        return cmp(self.get_interval(), other_item.get_interval())
    def matches(self, other_event, gold_equiv_clusters, approximate=False):
        # these are skipped until we're ready to do Task 2
        slots_to_skip = ['site', 'csite', 'toloc', 'atloc']
        if approximate == True:
            slots_to_skip.append('cause')
        if approximate != 'never':
            approximate = True

        # print 'matches()', approximate
        # print 'this =', self.pretty_format()
        # print 'other_event =', other_event.pretty_format()

        if not isinstance(other_event, Event):
            # print 'no match: not an event'
            return False

        equivalent_terms_anchors = gold_equiv_clusters.get(self.event_term.id, [self.event_term])
        matched_anchor = False
        for term in equivalent_terms_anchors:
            if term.matches(other_event.event_term, gold_equiv_clusters, approximate=approximate):
                matched_anchor = True
                break
        if not matched_anchor:
            # print 'no match: anchor'
            return False

        unmatched_other_event_args = set()
        for other_arg_type, other_arg_value in other_event.args:
            if normalize_arg_type(other_arg_type) in slots_to_skip:
                continue
            else:
                unmatched_other_event_args.add((other_arg_type, other_arg_value))

        for arg_type, arg_value in self.args:
            if normalize_arg_type(arg_type) in slots_to_skip:
                continue

            equivalent_terms_args = gold_equiv_clusters.get(arg_value.id, [arg_value])
            matched_arg = False
            for arg_value in equivalent_terms_args:
                for other_arg_type, other_arg_value in unmatched_other_event_args:
                    if normalize_arg_type(other_arg_type) in slots_to_skip:
                        continue

                    if normalize_arg_type(arg_type) != normalize_arg_type(other_arg_type):
                        continue
                    if arg_value.matches(other_arg_value, gold_equiv_clusters, approximate=approximate):
                        # print 'matched arg', other_arg_value.pretty_format()
                        matched_arg = True
                        unmatched_other_event_args.remove((other_arg_type, other_arg_value))
                        break
                if matched_arg:
                    break

            if not matched_arg:
                # print 'no match: argument', arg_value.pretty_format()
                return False

        if unmatched_other_event_args:
            # print 'no match: unmatched_other_event_args'
            return False

        return True
    def expand_token_offsets(self, document_text, text_picture):
        self.event_term.expand_token_offsets(document_text, text_picture)
        for relation_name, event in self.args:
            event.expand_token_offsets(document_text, text_picture)
    def resolve(self, id_mapping):
        self.event_term = id_mapping[self.event_term]
        self.args = [(k, id_mapping[v]) for (k, v) in self.args]

    __repr__ = generic_repr

    def pretty_format(self, depth=0):
        sep = '\n' + '\t' * (depth + 1)
        args = sep.join('%s:%s' % (relation_name, arg.pretty_format(depth + 1))
            for relation_name, arg in sorted(self.args))
        if args:
            args = sep + args
        return '%s%s' % (self.event_term.pretty_format(depth + 1), args)

def bionlp_items_from_string(docid, input_string):
    """Need docid so we can fetch the a1 entities."""
    events_db = get_events_db()
    gold_objs = events_db.get_items_in_document(docid)
    entities = [obj for obj in gold_objs if isinstance(obj, Term) and obj.given]

    objs = events_db.read_stream(None, input_string.splitlines())

    all_objs = entities + list(objs)
    id_mapping = {}
    for obj in all_objs:
        id_mapping[obj.id] = obj

    for obj in all_objs:
        obj.resolve(id_mapping)

    return all_objs

class BioNLPEvents:
    """Database of BioNLP events.  Ignores Modal for now."""
    def __init__(self):
        self.id_mappings = {} # doc ID : { event/term ID : Event / Term object }
        self.pmid_to_text = {} # doc ID : .txt filename
    def docid_iterator(self):
        for docid in sorted(self.id_mappings):
            yield docid
    def get_items_in_document(self, docid):
        id_mapping = self.id_mappings[docid]
        return id_mapping.values()
    def get_events_and_equiv_clusters(self, docid):
        events = []
        terms_to_clusters = {}
        id_mapping = self.id_mappings[docid]
        for id, item in id_mapping.items():
            if isinstance(item, Event):
                events.append(item)
            elif isinstance(item, Equiv):
                cluster = set(item.terms)
                for term in item.terms:
                    terms_to_clusters[term.id] = cluster

                if 0:
                    cluster = terms_to_clusters.get(item.term1) or terms_to_clusters.get(item.term2)
                    if not cluster:
                        cluster = set()
                        terms_to_clusters[item.term1.id] = cluster
                        terms_to_clusters[item.term2.id] = cluster
                    cluster.add(item.term1)
                    cluster.add(item.term2)

        events = get_unique_events(events, terms_to_clusters)
        return events, terms_to_clusters
    def read(self, basedir, just_a1_files=False, just_a2_files=False):
        basedir = path(basedir)
        basenames = set(f.stripext() for f in basedir.files('*.a1'))
        if just_a2_files:
            basenames.update(set(f.stripext() for f in basedir.files('*.a2')))
        for basename in basenames:
            pmid = basename.basename()
            self.id_mappings[pmid] = self.read_basename(pmid, basedir/basename, just_a1_files, just_a2_files)
            if not just_a2_files:
                self.pmid_to_text[pmid] = basedir/basename + '.txt'
    def read_basename(self, pmid, basename, just_a1_files, just_a2_files):
        if just_a2_files:
            entities = []
        else:
            entities = self.read_entities(basename)
        if just_a1_files:
            rest = []
        else:
            rest = self.read_rest(basename)
        all_objs = entities + rest
        id_mapping = self.id_mappings.get(pmid, {})
        for obj in all_objs:
            id_mapping[obj.id] = obj

        for obj in all_objs:
            obj.resolve(id_mapping)

        return id_mapping
    def read_term(self, basename, line, given=False):
        """If given=True, the entity comes from an .a1 file and doesn't
        need to be predicted or scored."""
        pieces = multisplit(line.strip(), [' ', '\t'])
        pieces[4:] = [' '.join(pieces[4:])]
        pieces.append(given)
        return Term(basename, *pieces)
    def read_event(self, basename, line):
        pieces = multisplit(line.strip(), [' ', '\t'])
        args = [piece.split(':') for piece in pieces[2:]]
        del pieces[2:]
        event_type, event_term = pieces[1].split(':')
        pieces[1] = event_type
        pieces.insert(2, event_term)
        pieces.append(args)
        return Event(basename, *pieces)
    def read_star(self, basename, line):
        pieces = multisplit(line.strip(), [' ', '\t'])
        assert pieces[1] == 'Equiv'
        return Equiv(basename, *pieces[2:])
    def read_entities(self, basename):
        terms = []
        filename = basename + '.a1'
        for line in file(filename):
            terms.append(self.read_term(basename, line, given=True))

        return terms
    def read_stream(self, basename, stream):
        dispatch = {'*': self.read_star,
                    'T': self.read_term,
                    'E': self.read_event,
                    'M': None # XXX for now
        }
        for line in stream:
            reader_func = dispatch.get(line[0])
            if reader_func:
                yield reader_func(basename, line)
    def read_rest(self, basename):
        filename = basename + '.a2'
        if not path(filename).exists():
            return []
        items = list(self.read_stream(basename, file(filename)))

        # items = [item for item in items if isinstance(item, Equiv)]
        # items.extend(self.read_stream(basename, file(filename + '.t1')))

        return items

def real_evaluation_bionlp_components_genia09(test_dir, gold_dir, show_output=False):
    import commands, re
    command = 'eval_approx_recursive %s %s' % (gold_dir, test_dir)
    output = commands.getoutput(command)
    if show_output:
        print output
    scores_re = re.compile(r'==\[ALL-TOTAL\]==\s+(\d+) \(\s+(\d+)\)\s+(\d+)')
    scores_re = re.compile(r'==\[ALL-TOTAL\]==\s+(\d+) \(\s+(\d+)\)\s+(\d+) \(\s+(\d+)\)')
    gold, matched, proposed, matched2 = [int(i) for i in scores_re.findall(output)[0]]
    # how is it that matched can be diferent from matched2?  probably bugs in their evaluator!
    return (matched2, gold, proposed)

def real_evaluation_bionlp_components(test_dir, gold_dir, show_output=False, verbose=False):
    import tempfile
    import commands, re

    normalized_test_dir = path(tempfile.mkdtemp(prefix='a2-norm-'))
    norm_command = '/u/nlp/data/bioNLP/2011/tools/a2-normalize.pl -g %s -o %s %s' % \
        (gold_dir, normalized_test_dir, test_dir)
    output = commands.getoutput(norm_command)
    # print 'norm_command', norm_command
    # print 'norm output', repr(output)

    flags = '-v'
    if verbose:
        flags += ' -x'
    command = '/u/nlp/data/bioNLP/2011/tools/a2-evaluate.pl %s -g %s -t1 -sp %s/*.a2' % (flags, gold_dir, 
        normalized_test_dir)
    output = commands.getoutput(command)
    # print 'command', repr(command)
    if show_output:
        print 'Eval output:'
        print output
    scores_re = re.compile(r'==\[ALL-TOTAL\]==\s+(\d+) \(\s+(\d+)\)\s+(\d+)')
    scores_re = re.compile(r'==\[ALL-TOTAL\]==\s+(\d+) \(\s+(\d+)\)\s+(\d+) \(\s+(\d+)\)')
    gold, matched, proposed, matched2 = [int(i) for i in scores_re.findall(output)[0]]
    # how is it that matched can be diferent from matched2?  probably bugs in their evaluator!
    return (matched2, gold, proposed)

def per_sentence_bionlp_fscores_nbest(test_filenames, gold_dir):
    import tempfile
    from DataSet import DataSet
    from cStringIO import StringIO

    gold_dir = path(gold_dir)

    sentences = DataSet.from_filenames(*test_filenames)
    for docid, sentences_in_doc in sentences.group_by_metadata('DOC'):
        # if docid != '9361029':
            # continue

        print 'DOC:', docid
        our_total_proposed = 0
        our_total_matched = 0
        for i, sentence in enumerate(sentences_in_doc):
            for j, parse in enumerate(sentence):
                print "DOC:", docid, 'Sentence:', i, 'Parse:', j
                our_score_components = parse.bionlp_fscore_components(sentence)
                matched, gold, proposed = our_score_components
                our_total_proposed += proposed
                our_total_matched += matched

                conll_version = StringIO()
                parse.write_conll(conll_version, include_metadata=False, sentence=sentence)
                conll_version.seek(0)
                conll_version = conll_version.read()

                import BioNLPConversionDB
                converter = BioNLPConversionDB.get_converter()
                bionlp_events_string = converter.convert(conll_version)

                if 0:
                    print 'Events ---'
                    print bionlp_events_string
                    print 'Events ---'

                temp_test_dir = path(tempfile.mkdtemp(prefix=docid + '-'))
                temp_test_filename = path(temp_test_dir/docid + '.a2.t1')
                temp_test_file = file(temp_test_filename, 'w')
                temp_test_file.write(bionlp_events_string)
                temp_test_file.close()
                real_score_components = real_evaluation_bionlp_components(temp_test_dir, gold_dir)
                if our_score_components != real_score_components:
                    real_evaluation_bionlp_components(temp_test_dir, gold_dir, show_output=True)
                temp_test_dir.rmtree()
                if our_score_components != real_score_components:
                    print "Ours:", our_score_components
                    print 'Real:', real_score_components 

                    print 'Events ---'
                    print bionlp_events_string
                    print 'Events ---'
                    raise 'mismatch'

def per_sentence_bionlp_fscores(test_filename, test_dir, gold_dir):
    import tempfile
    from DataSet import DataSet
    from cStringIO import StringIO
    test_dir = path(test_dir)
    gold_dir = path(gold_dir)

    sentences = DataSet.from_filenames(test_filename)
    for docid, sentences_in_doc in sentences.group_by_metadata('DOC'):
        # if docid != '9015187':
        # if docid != '9081693':
        # if docid != '9257843':
        # if docid != '8108127':
        # if docid != '9115366':
        # if docid != '9361029':
            # continue

        print 'DOC:', docid
        our_total_proposed = 0
        our_total_matched = 0
        for sentence in sentences_in_doc:
            parse = sentence.gold_parse
            sentence.parses = [parse]
            our_score_components = parse.bionlp_fscore_components(sentence)
            matched, gold, proposed = our_score_components
            our_total_proposed += proposed
            our_total_matched += matched

            conll_version = StringIO()
            parse.write_conll(conll_version, include_metadata=False, sentence=sentence)
            conll_version.seek(0)
            conll_version = conll_version.read()

            import BioNLPConversionDB
            converter = BioNLPConversionDB.get_converter()
            bionlp_events_string = converter.convert(conll_version)

            if 1:
                print 'Events ---'
                print bionlp_events_string
                print 'Events ---'
            
            print "Ours:", our_score_components

            temp_test_dir = path(tempfile.mkdtemp(prefix=docid + '-'))
            temp_test_filename = path(temp_test_dir/docid + '.a2.t1')
            temp_test_file = file(temp_test_filename, 'w')
            temp_test_file.write(bionlp_events_string)
            temp_test_file.close()
            real_score_components = real_evaluation_bionlp_components(temp_test_dir, gold_dir)
            print 'Real:', real_score_components 
            if our_score_components != real_score_components:
                real_evaluation_bionlp_components(temp_test_dir, gold_dir, show_output=True)
            temp_test_dir.rmtree()
            if our_score_components != real_score_components:
                raise 'mismatch'

def test():
    basedir = path('/u/nlp/data/bioNLP/bioNLP09-09082010-ensemble-triggers')
    events_db = get_events_db(basedir)
    events = events_db.get_items_in_document(10089566)
    for event in events:
        print event

if __name__ == "__main__":
    # test()
    if 0:
        per_sentence_bionlp_fscores('/u/mcclosky/res/ie/bionlp/hog/dev-rerank-2P.2/outputs/2/after.conll', 
                                    '/u/mcclosky/res/ie/bionlp/hog/dev-rerank-2P.2/bionlp/4/', 
                                    '/u/nlp/data/bioNLP/current/development/')
    elif 0:
        per_sentence_bionlp_fscores('/u/mcclosky/res/ie/bionlp/hog/rerank-2P-bionlp2-trigger0/outputs/2/after.conll',
                                    '/u/mcclosky/res/ie/bionlp/hog/rerank-2P-bionlp2-trigger0/bionlp/4/',
                                    '/u/nlp/data/bioNLP/current/folds/13-15/')
    elif 1:
        per_sentence_bionlp_fscores_nbest(['/juice/u30/u/mcclosky/res/ie/bionlp/hog/reranker_train_trigger/run/12/testing.conll.gz',
                                           '/juice/u30/u/mcclosky/res/ie/bionlp/hog/reranker_train_trigger/run/12/testing.out.gz'],
                                    '/u/nlp/data/bioNLP/current/folds/00/')
