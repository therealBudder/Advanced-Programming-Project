using Avalonia;
using Avalonia.Controls;
using Avalonia.Interactivity;
using System.Diagnostics;

namespace AdProgrammingGUIAvalonia.Views;

public partial class MainView : UserControl
{
    public MainView()
    {
        InitializeComponent();
    }

    public void ButtonClicked(object source, RoutedEventArgs args)
    {
        var button = (source as Button)!;
        switch (button.Name)
        {
            case "reset":
                {
                    var equationBox = this.Find<TextBox>("equationInput") as TextBox;
                    equationBox.Text = string.Empty;
                }
                break;
            case "compute":
                button.Content = "lol";
                break;
        }
    }
}
