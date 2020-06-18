##################
# py script test #
##################

import re


# fun to be applied
def apply_regexprs(vect,
                   regexprs):
    
    # controls
    vect_type = type(vect).__name__
    if vect is None or not vect_type in ["Series", "str", "list"]:
        raise Exception("vect must be of class <Series>, <str> or <list>.")
    else:
        if vect_type == 'Series':
            vect = list(vect)
    if regexprs is None or not type(regexprs).__name__ in ["dict", "list"]:
        raise Exception("regexprs must be of class <dict> or <list>")

    # compile regexpr to save time (2-3 times faster)
    compiled_reg = [re.compile(x) for x in regexprs]

    # handle case with multiple elements in vect
    def map_fun(vect_):
        for reg, val in zip(compiled_reg, regexprs.values()):
            if vect_ is not None:
                vect_ = reg.sub(val, vect_)
        return vect_

    # handle case when only one element in regexprs and vect
    if type(vect).__name__ == 'str':
        vect = [vect]

    # apply all regexprs on each element of vect
    vect = list(map(map_fun, vect))

    if vect_type == 'Series':
        vect = pd.Series(vect)

    return vect
