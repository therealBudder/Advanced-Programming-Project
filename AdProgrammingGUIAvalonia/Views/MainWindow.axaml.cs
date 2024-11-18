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
        window.MinWidth = 512;
        window.MinHeight = 380;
        window.Width = 1024;
        window.Height = 768;
    }


}
