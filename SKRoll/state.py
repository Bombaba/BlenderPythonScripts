import re
import collections.abc
import time

PROP_STATE = '_state_'
PROP_PREVSTATE = '_previous_state_'
PROP_STATE_SEQ = '_state_sequence_'
PROP_SEQ_INDEX = '_sequence_index_'
PROP_STATE_FRAME = '_state_frame_'
PROP_STATE_START_TIME = '_state_time_'
PROP_TIMER = '_timer_'


MASTER_ID = 'M_'

def get_property(ctrlr, name):
    return ctrlr.owner[name]

def set_property(ctrlr, name, value):
    owner = ctrlr.owner
    assert name in owner
    owner[name] = value

def increment_property(ctrlr, name):
    ctrlr.owner[name] += 1
def decrement_property(ctrlr, name):
    ctrlr.owner[name] -= 1

def set_sequence(ctrlr, *seq):
    assert seq[-1].startswith(MASTER_ID)
    owner = ctrlr.owner
    owner[PROP_STATE_SEQ][:] = seq
    owner[PROP_SEQ_INDEX] = -1

def next_state(ctrlr):
    owner = ctrlr.owner
    owner[PROP_SEQ_INDEX] += 1
    _change_state(owner, owner[PROP_STATE_SEQ][owner[PROP_SEQ_INDEX]])

def prev_state(ctrlr):
    owner = ctrlr.owner
    owner[PROP_SEQ_INDEX] -= 1
    _change_state(owner, owner[PROP_STATE_SEQ][owner[PROP_SEQ_INDEX]])

def goto_master(ctrlr, master_state):
    assert master_state.startswith(MASTER_ID)
    owner = ctrlr.owner
    _change_state(owner, master_state)
    del owner[PROP_STATE_SEQ][:]
    owner[PROP_SEQ_INDEX] = -1

def statetime(ctrlr):
    return time.perf_counter() - ctrlr.owner[PROP_STATE_START_TIME]

def stateframe(ctrlr):
    return ctrlr.owner[PROP_STATE_FRAME]

def timer_set(ctrlr):
    ctrlr.owner[PROP_TIMER] = timer.perf_counter()

def timer_get(ctrlr):
    return time.perf_counter() - ctrlr.owner[PROP_TIMER]

def do_nothing(ctrlr):
    pass


class StateFuncsMeta(type):
    statefunc_pattern = re.compile(
        """
        ([A-Z][A-Z0-9_]+)      # the state name, all upper case
        _                     # underscore
        (init|main|clean)       # the func role
        $                     #
        """, re.VERBOSE)
    def __new__(mcls, name, bases, namespace, **kwargs):
        result = type.__new__(mcls, name, bases, namespace, **kwargs)
        assert result._initial_.isupper() and \
                   result._initial_.startswith(MASTER_ID)
        assert isinstance(result._properties_, dict)

        result._states = dict()
        result._user_props = result._properties_.copy()
        for base in bases:
            if isinstance(base, mcls):
                result._states.update(base._states)
                result._user_props.update(base._user_props)

        state_names = set()
        for attrname in namespace:
            match = mcls.statefunc_pattern.match(attrname)
            if match:
                assert isinstance(namespace[attrname],
                                  collections.abc.Callable)
                state_names.add(match.group(1))

        for sname in state_names:
            result._states[sname] = (
                getattr(result, sname + "_init", do_nothing),
                getattr(result, sname + "_main", do_nothing),
                getattr(result, sname + "_clean", do_nothing),
                )

        return result

    @staticmethod
    def main(cls, ctrlr):
        owner = ctrlr.owner
        current_state = owner[PROP_STATE]
        owner[PROP_STATE_FRAME] += 1
        if current_state in cls._states:
            statefunc_init, statefunc_main, statefunc_clean = \
                cls._states[current_state]
            #if previous_state != current_state:
            if owner[PROP_STATE_FRAME] == 1:
                statefunc_init(ctrlr)
                owner[PROP_PREVSTATE] = current_state
            statefunc_main(ctrlr)
            cls.INTERRUPT(ctrlr, statefunc_clean)
        else:
            cls.INTERRUPT(ctrlr, do_nothing)

    def __call__(cls):
        def loop(ctrlr):
            owner = ctrlr.owner
            if PROP_STATE not in owner:
                owner[PROP_STATE] = cls._initial_
                owner[PROP_PREVSTATE] = ''
                owner[PROP_STATE_SEQ] = list()
                owner[PROP_SEQ_INDEX] = -1
                owner[PROP_STATE_FRAME] = 0
                owner[PROP_STATE_START_TIME] = \
                    owner[PROP_TIMER] = time.perf_counter()
                for name, value in cls._user_props.items():
                    owner[name] = value
            __class__.main(cls, ctrlr)
        return loop
            

class StateFuncs(metaclass=StateFuncsMeta):
    _initial_ = 'M_EMPTY'
    _properties_ = {}
    def M_EMPTY_main(ctrlr):
        pass

    def INTERRUPT(ctrlr, statefunc_clean):
        pass

def _change_state(owner, new_state):
    assert new_state.isupper()
    owner[PROP_PREVSTATE] = owner[PROP_STATE]
    owner[PROP_STATE] = new_state
    owner[PROP_STATE_FRAME] = 0
    owner[PROP_STATE_START_TIME] = time.perf_counter()



if __name__ == '__main__':
    class TestStateFuncs(StateFuncs):
        _initial_ = 'M_INITIAL'
        def M_INITIAL_init(ctrlr):
            print("INITIAL_init function")
        def M_INITIAL_main(ctrlr):
            print("INITIAL_main function")
        def M_INITIAL_clean(ctrlr):
            print("INITIAL_clean function")
        def NEXT_STATE_main(ctrlr):
            print("NEXT_STATE_main function")

    class TestController:
        def __init__(self):
            self.owner = dict()

    c = TestController()
    #loop = TestStateFuncs.get_loopfunc('M_INITIAL')
    #loop(c)
    #print(c.owner)
    #loop(c)
    #print(c.owner)
    TestStateFuncs(c)
    print(c.owner)

