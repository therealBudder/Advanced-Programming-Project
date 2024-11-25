using OxyPlot;
using System.Collections.Generic;

namespace AdProgrammingGUIAvalonia.ViewModels;

public class MainViewModel : ViewModelBase
{
    public string EquationInputText => "Enter equation";
    public string ResultText => "Result";
    public string HistoryText => "Awaiting results";

    public string ErrorText => "Awaiting errors";

    public MainViewModel()
    {
        this.Title = "Example 2";
        this.Points = new List<DataPoint> { new DataPoint(0, 0) };
    }

    public void AddPoint(DataPoint point)
    {
        this.Points.Add(point);
    }

    public string Title { get; private set; }

    public List<DataPoint> Points { get; private set; }
}