class ParseError(Exception):
    pass

class Sentence(object):

    def __init__(self, subject, verb, obj):
        # remember we take ('noun', 'princess') tuples and convert them
        self.subject = subject[1]
        self.verb = verb[1]
        self.obj = obj[1]

def peek(word_list):
    """return type of first (type, word) tuple from list"""
    if word_list:
        word = word_list[0]
        return word[0]
#        return word[0]

    else:
        return None

def match(word_list, expecting):
    """Return word from first (type, word) tuple if type matches expecting"""
    if word_list:
        word = word_list.pop(0)

        if word[0] == expecting:
            return word
        else:
            return None
    else:
        return None

def skip(word_list, word_type):
    """Remove (type, word) tuples of type word_type from word_list"""
    while peek(word_list) == word_type:
        match(word_list, word_type)

def parse_verb(word_list):
    """Get verb from word_list"""
    skip(word_list, 'stop')

    if peek(word_list) == 'verb':
        return match(word_list, 'verb')
    else:
        raise ParseError("Expected a verb next.")

def parse_object(word_list):
    """Get object from word_list"""
    skip(word_list, 'stop')
    next = peek(word_list)

    if next == 'noun':
        return match(word_list, 'noun')
    if next == 'direction':
        return match(word_list, 'direction')
    else:
        raise ParseError("Expected a noun or direction next.")

def parse_subject(word_list, subj):
    verb = parse_verb(word_list)
    obj = parse_object(word_list)

    return Sentence(subj, verb, obj)

def parse_sentence(word_list):
    skip(word_list, 'stop')

    start = peek(word_list)

    if start == 'noun':
        subj = match(word_list, 'noun')
        return parse_subject(word_list, subj)
    elif start == 'verb':
        # assume the subject is the player then
        return parse_subject(word_list, ('noun', 'player'))
    else:
        raise ParseError("Must start with subject, object, or verb not: %s" % start)
    
