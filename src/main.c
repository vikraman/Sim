#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include "callbacks.h"

gint
main (gint argc, gchar ** argv)
{
  GtkWidget * window = NULL;
  GtkWidget * vbox = NULL;
  GtkWidget * menubar = NULL;
  GtkWidget * filemenu = NULL;
  GtkWidget * file = NULL;
  GtkWidget * new = NULL;
  GtkWidget * quit = NULL;
  GtkWidget * helpmenu = NULL;
  GtkWidget * help = NULL;
  GtkWidget * about = NULL;

  u_int8_t e = 14;

  GtkAccelGroup * accel_group;

  gtk_init (&argc, &argv);

  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_widget_add_events (window, GDK_BUTTON_PRESS_MASK);
  gtk_window_set_position (GTK_WINDOW (window), GTK_WIN_POS_CENTER);
  gtk_window_set_title (GTK_WINDOW (window), "Sim");
  gtk_widget_set_size_request (window, 480, 640);
  gtk_window_set_resizable (GTK_WINDOW (window), FALSE);

  vbox = gtk_vbox_new (FALSE, 0);
  gtk_container_add (GTK_CONTAINER (window), vbox);

  accel_group = gtk_accel_group_new ();
  gtk_window_add_accel_group (GTK_WINDOW (window), accel_group);

  menubar = gtk_menu_bar_new ();

  filemenu = gtk_menu_new ();
  file = gtk_menu_item_new_with_mnemonic ("_File");
  new = gtk_image_menu_item_new_from_stock (GTK_STOCK_NEW, accel_group);
  gtk_widget_add_accelerator (new, "activate", accel_group,
			      GDK_n, GDK_CONTROL_MASK, GTK_ACCEL_VISIBLE);
  quit = gtk_image_menu_item_new_from_stock (GTK_STOCK_QUIT, accel_group);
  gtk_widget_add_accelerator (quit, "activate", accel_group,
			      GDK_q, GDK_CONTROL_MASK, GTK_ACCEL_VISIBLE);
  gtk_menu_shell_append (GTK_MENU_SHELL (filemenu), new);
  gtk_menu_shell_append (GTK_MENU_SHELL (filemenu), quit);

  helpmenu = gtk_menu_new ();
  help = gtk_menu_item_new_with_mnemonic ("_Help");
  about = gtk_image_menu_item_new_from_stock (GTK_STOCK_ABOUT, accel_group);
  gtk_widget_add_accelerator (about, "activate", accel_group,
			      GDK_F1, 0, GTK_ACCEL_VISIBLE);
  gtk_menu_shell_append (GTK_MENU_SHELL (helpmenu), about);

  gtk_menu_item_set_submenu (GTK_MENU_ITEM (file), filemenu);
  gtk_menu_item_set_submenu (GTK_MENU_ITEM (help), helpmenu);
  gtk_menu_shell_append (GTK_MENU_SHELL (menubar), file);
  gtk_menu_shell_append (GTK_MENU_SHELL (menubar), help);
  gtk_box_pack_start (GTK_BOX (vbox), menubar, FALSE, FALSE, 0);

  g_signal_connect_swapped (G_OBJECT (new), "activate",
			    G_CALLBACK (draw_board), window);

  g_signal_connect (G_OBJECT (quit), "activate",
		    G_CALLBACK (gtk_main_quit), NULL);

  g_signal_connect_swapped (G_OBJECT (about), "activate",
			    G_CALLBACK (show_about_dialog), window);

  g_signal_connect (G_OBJECT (window), "button-press-event",
		    G_CALLBACK (draw_edge), &e);

  g_signal_connect_swapped (G_OBJECT (window), "delete_event",
			    G_CALLBACK (gtk_main_quit), NULL);

  gtk_widget_show_all (window);
  gtk_main ();

  return EXIT_SUCCESS;
}
