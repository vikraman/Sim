#ifndef CALLBACKS_H
#define CALLBACKS_H
#include <gtk/gtk.h>

void show_about_dialog (GtkWidget *, gpointer);
void start_game (GtkWidget *, gpointer);
void draw_board (GtkWidget *, gpointer);
void draw_edge (GtkWidget *, GdkEvent *, gpointer);

#endif
