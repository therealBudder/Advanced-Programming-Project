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
        window.MinWidth = 800;
        window.MinHeight = 450;
        window.Width = 1280;
        window.Height = 720;
    }


}
