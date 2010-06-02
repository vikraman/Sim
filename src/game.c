#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <sys/types.h>
#include "game.h"

int start_game_loop () {
  MOVE e; int in, i, j;
  LIST A1 = init();
  LIST A2 = init();
  LIST B1 = init();
  LIST B2 = init();
  LIST AC = init();
  fill (A2);
  fill (B2);

  do {
    cost (A1, A2, AC);
    printf ("A2: "); disp (A2);
    printf ("AC: "); for (i=1;i<=6;i++) for (j=i+1;j<=6;j++) if (A2[i][j]) printf ("%02d  ", AC[i][j]); printf ("\n");
    e = getmove (AC, A2, B2);
    printf ("A's move : %02d\n", e);
    add (A1, e);
    del (A2, e);
    del (B2, e);
    prune (A1, A2);
    printf ("A1: "); disp (A1);
    printf ("A2: "); disp (A2);
    printf ("B1: "); disp (B1);
    printf ("B2: "); disp (B2);
    if (size(B2) == 0) {
      printf ("A wins \\o/\n");
      return EXIT_SUCCESS;
    }
    printf ("B's move : ");
    scanf ("%d", &in);
    e = in & 0xFF;
    add (B1, e);
    del (A2, e);
    del (B2, e);
    prune (B1, B2);
    printf ("A1: "); disp (A1);
    printf ("A2: "); disp (A2);
    printf ("B1: "); disp (B1);
    printf ("B2: "); disp (B2);
    if (size(A2) == 0) {
      printf ("B wins \\o/\n");
      return EXIT_SUCCESS;
    }
  } while (e != 0x00);

  destruct (A1);
  destruct (A2);
  destruct (B1);
  destruct (B2);
  destruct (AC);
  return EXIT_SUCCESS;
}

LIST init (void) {
  LIST list;
  u_int8_t i;
  list = malloc (7 * sizeof (u_int8_t *));
  for (i = 0; i < 7; i++)
    list[i] = calloc (7, sizeof (u_int8_t));
  return list;
}

void fill (LIST list) {
  u_int8_t i, j;
  for (i = 1; i <= 6; i++)
    for (j = i+1; j <=6; j++)
      list[i][j] =1;
}

void destruct (LIST list) {
  u_int8_t i;
  for (i = 0; i < 7; i++)
    free (list[i]);
  free (list);
}

bool add (LIST list, MOVE move) {
  if (list[move/10][move%10])
    return false;
  list[move/10][move%10] = 1;
  return true;
}

bool del (LIST list, MOVE move) {
  if (!list[move/10][move%10])
    return false;
  list[move/10][move%10] = 0;
  return true;
}

void disp (LIST list) {
  u_int8_t i, j;
  for (i = 1; i <= 6; i++)
    for (j = i+1; j <= 6; j++)
      if (list[i][j])
	printf ("%1d%1d  ", i, j);
  printf ("\n");
}

/* prune moves from l2 which when added to l1 will form a triangle, return no. of pruned moves */
u_int8_t prune (LIST l1, LIST l2) {
  u_int8_t i1, j1, i2, j2;
  u_int8_t count = 0;
  for (i1=1; i1<=6; i1++)
    for (j1=i1+1; j1<=6; j1++)
      for (i2=1; i2<=6; i2++)
	for (j2=i2+1; j2<=6; j2++) {
	  if (i1 == i2 && j1 == j2)
	    continue;
	  /* extremely scary, I won't remember how I did it later :-( */
	  if (l1[i1][j1] && l1[i2][j2]) {
	    if (i1 == i2 && (l2[j1][j2] || l2[j2][j1])) {
      	      l2[j1][j2] = l2[j2][j1] = 0;
	      count++;
	    }
	    if (i1 == j2 && (l2[i2][j1] || l2[j1][i2])) {
	      l2[i2][j1] = l2[j1][i2] = 0;
	     count++;
	    }
	    if (i2 == j1 && (l2[i1][j2] || l2[j2][i1])) {
	      l2[i1][j2] = l2[j2][i1] = 0;
	      count++;
	    }
	    if (j1 == j2 && (l2[i1][i2] || l2[i2][i1])) {
	      l2[i1][i2] = l2[i2][i1] = 0;
	      count++;
	    }
	  }
	}
  return count;
}

/* compute ne for all e in l2 and fill into l3
 * reads from l1 and l2, writes to l3 */
void cost (LIST l1, LIST l2, LIST l3) {
  u_int8_t i, j, m, n;
  LIST temp;
  temp = init();
  /* got a copy of l2 */
  for (i = 1; i <= 6; i++)
    for (j = i+1; j <= 6; j++)
      if (l2[i][j]) { /* for e in l2 */
	l1[i][j] = 1; /* add e to l1 */
	for (m = 1; m <= 6; m++) /* copy l2 to temp */
	  for (n = m+1; n <= 6; n++)
	    temp[m][n] = l2[m][n];
	l3[i][j] = prune (l1, temp); /* prune temp not l2 to compute ne */
	l1[i][j] = 0; /* reset l1 */
      }
  destruct (temp);
}

MOVE getmove (LIST cost, LIST me, LIST opponent) {
  u_int8_t i, j, nas = 0, nme = 0, nopponent = 0;
  LIST AS = init ();
  MOVE e;
  u_int8_t minn = 0xFF;
  int8_t fe, minf = 0x7F;
  for (i = 1; i <= 6; i++)
    for (j = i+1; j <= 6; j++) {
      if (me[i][j]) {
	fe = cost[i][j] - opponent[i][j];
	if (fe <= minf)
	  minf = fe;
	if (cost[i][j] <= minn)
	  minn = cost[i][j];
	nme++;
      }
      if (opponent[i][j])
	nopponent++;
    }
  for (i = 1; i <= 6; i++)
    for (j = i+1; j <= 6; j++)
      if (me[i][j])
	if (minf == (int8_t)(cost[i][j] - opponent[i][j])) {
	  AS[i][j] = 1;
	  nas++;
	}
  if (nas == 1)
    for (i = 1; i <= 6; i++)
      for (j = i+1; j <= 6; j++)
	if (AS[i][j]) {
	  e = i*10+j;
	  destruct (AS);
	  return e;
	}
  if (nme > nopponent)
    for (i = 1; i <= 6; i++)
      for (j = i+1; j <= 6; j++)
	if (AS[i][j] && opponent[i][j]) {
	  e = i*10+j;
	  destruct (AS);
	  return e;
	}
  for (i = 1; i <= 6; i++)
    for (j = i+1; j <= 6; j++)
      if (AS[i][j] && minn == cost[i][j]) {
	e = i*10+j;
	destruct (AS);
	return e;
      }
  destruct (AS);
  return 12;
}

size_t size (LIST list) {
  int i, j;
  size_t size = 0;
  for (i = 1; i <= 6; i++)
    for (j = i+1; j <= 6; j++)
      size += list[i][j];
  return size;
}
