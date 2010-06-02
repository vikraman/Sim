#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <gtk/gtk.h>
#include <cairo.h>
#include <math.h>
#include <stdio.h>
#include "callbacks.h"
#include "game.h"

#define SQRT_3 1.7320508075

double ex[] = { 135, 345, 450, 345, 135, 30 };
double ey[] = { 140, 140, 140 + 105*SQRT_3, 140 + 210*SQRT_3, 140 + 210*SQRT_3, 140 + 105*SQRT_3 };

void
show_about_dialog (GtkWidget * widget, gpointer data)
{
  GtkWidget * dialog = gtk_about_dialog_new ();

  gtk_about_dialog_set_program_name (GTK_ABOUT_DIALOG (dialog), PACKAGE_NAME);
  gtk_about_dialog_set_version (GTK_ABOUT_DIALOG (dialog), PACKAGE_VERSION);
  gtk_about_dialog_set_copyright (GTK_ABOUT_DIALOG (dialog), "(c) Vikraman Choudhury");
  gtk_about_dialog_set_comments (GTK_ABOUT_DIALOG (dialog), "A simple pencil game");
  gtk_about_dialog_set_website (GTK_ABOUT_DIALOG (dialog), PACKAGE_URL);
  gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
  gtk_window_set_transient_for (GTK_WINDOW (dialog), GTK_WINDOW (widget));

  gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);
}

void
draw_board (GtkWidget * widget, gpointer data)
{
  cairo_t * cr = NULL;
  int i;
  char text[2];

  cr = gdk_cairo_create (widget->window);

  cairo_set_source_rgb (cr, 1.0, 0.0, 0.0);
  for (i = 0; i < 6; i++) {
    cairo_arc (cr, ex[i], ey[i], 10.0, 0.0, 2*M_PI);
    cairo_fill (cr);
  }

  cairo_set_source_rgb (cr, 0.0, 0.0, 0.0);
  cairo_select_font_face (cr, "Purisa",
			  CAIRO_FONT_SLANT_NORMAL,
			  CAIRO_FONT_WEIGHT_BOLD);
  for (i = 0; i < 6; i++) {
    sprintf (text, "%d", i+1);
    cairo_move_to (cr, ex[i]-3, ey[i]+4);
    cairo_show_text (cr, text);
  }

  cairo_destroy (cr);
}

void
draw_edge (GtkWidget * widget, GdkEvent * event, gpointer data)
{
  gint8 e = * (gint8 *)data;
  cairo_t * cr = NULL;

  cr = gdk_cairo_create (widget->window);

  cairo_set_source_rgba (cr, 0.0, 0.0, 1.0, 0.5);
  cairo_set_line_cap (cr, CAIRO_LINE_CAP_ROUND);

  cairo_move_to (cr, ex[e/10-1], ey[e/10-1]);
  cairo_line_to (cr, ex[e%10-1], ey[e%10-1]);

  cairo_stroke (cr);
  cairo_destroy (cr);
}
