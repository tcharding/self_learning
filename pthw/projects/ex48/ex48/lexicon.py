import re

directions = ['north', 'south', 'east', 'west', 'up', 'down', 'back', 'forword']
verbs = ['go', 'kill', 'eat']
stops = ['the', 'in', 'of']
nouns = ['bear', 'princess']

def scan(string):
    """Parses string returning tuples of ( type, word )
    supported types: direction verb stop noun number error"""
    words = string.split(' ')
    result = []
    for word in words:
        if word in directions:
            result.append(('direction', word))
        elif word in verbs:
            result.append(('verb', word))
        elif word in stops:
            result.append(('stop', word))
        elif word in nouns:
            result.append(('noun', word))
        elif re.match(r'\d', word):
            result.append(('number', word))
        else:
            result.append(('error', word))
            
    return result
