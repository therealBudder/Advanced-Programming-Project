using Avalonia.Controls;
using Avalonia.Interactivity;
using System.Diagnostics;

namespace AdProgrammingGUIAvalonia.Views;

public partial class MainWindow : Window
{
    public MainWindow()
    {
        InitializeComponent();
        var window = this;
        window.Title = "Calculator";
        window.MinWidth = 500;
        window.MinHeight = 250;
        window.Width = 800;
        window.Height = 600;
    }


}
