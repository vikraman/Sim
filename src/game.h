#ifndef GAME_H
#define GAME_H

#include <sys/types.h>

typedef u_int8_t MOVE;
typedef u_int8_t ** LIST;
typedef enum { false = 0, true = 1 } bool;

int start_game_loop ();
LIST init ();
void fill (LIST);
void destruct (LIST);
bool add (LIST, MOVE);
bool del (LIST, MOVE);
void disp (LIST);
u_int8_t prune (LIST, LIST);
size_t size (LIST);
void cost (LIST, LIST, LIST);
MOVE getmove (LIST, LIST, LIST);

#endif
