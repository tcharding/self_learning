from nose.tools import *
from ex48.Sentence import *

# test peek and match
wl = [('noun', 'bear'), ('verb', 'kill'), ('noun', 'princess')] 
assert_equal('noun', peek(wl))
assert_equal(match(wl, 'noun'), ('noun', 'bear'))

# test parse_subject

# test parse_verb
wl = [('verb', 'kill'), ('stop', 'the'), ('noun', 'princess')]
assert_equal(parse_verb(wl), ('verb', 'kill'))

# test parse_object
wl = [('noun', 'hat'), ('verb', 'kill'), ('stop', 'the'), ('noun', 'princess')]
assert_equal(parse_object(wl), ('noun', 'hat'))
wl = [('direction', 'south'), ('verb', 'kill'), ('stop', 'the')]
assert_equal(parse_object(wl), ('direction', 'south'))

# test parse_sentence, implicitly tests parse_subject
out = Sentence(('noun', 'tom'), ('verb', 'kill'), ('noun', 'bear'))
#wl = [('verb', 'kill'), ('stop', 'the'), ('noun', 'bear')]
got = parse_sentence([('noun', 'tom'), ('verb', 'kill'), ('noun', 'bear')])
assert_equal(out.subject, got.subject)
assert_equal(out.obj, got.obj)
assert_equal(out.verb, got.verb)

