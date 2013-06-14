CC = g++
ERL = erl
ERLC = erlc -I ${ERL_LIBS}
CCFLAGS = -Wall -Werror -I ${ERL_INTERFACE}/include -L${ERL_INTERFACE}/lib
LDFLAGS = -lerl_interface -lei -lpthread
OBJS = Main.o ErlComm.o 
BEAMS = dictionary_prop_tests.beam dictionary.beam cpp_comm.beam

ifeq ($(BUGS), 1)
	CCFLAGS += -DBUG1
endif
ifeq ($(BUGS), 2)
	CCFLAGS += -DBUG2
endif
ifeq ($(BUGS), 3)
	CCFLAGS += -DBUG1 -DBUG2
endif

.PHONY: all test clean

all: ${OBJS} ${BEAMS}
	${CC} ${CCFLAGS} ${OBJS} -o TestMain ${LDFLAGS}

%.o: %.cc
	${CC} -c $*.cc ${CCFLAGS}

%.beam: %.erl
	${ERLC} $*.erl

test:
	${ERL} -noshell -eval 'proper:quickcheck(dictionary_prop_tests:prop_dictionary_is_working(), 100), init:stop().'

clean:
	-rm -rf *.o *.beam *~ TestMain