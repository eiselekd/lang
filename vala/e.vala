
using Gtk;

public class HelloWorldWindow : Window {

    public HelloWorldWindow () {
        var label = new Label ("Hello World");
        add (label);
        set_default_size (100, 100);
    }
}

void main (string[] args) {
    Gtk.init (ref args);

    var win = new HelloWorldWindow ();
    win.destroy.connect (Gtk.main_quit);
    win.show_all ();

    Gtk.main ();
}