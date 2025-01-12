using Avalonia;
using Avalonia.Controls;
using Avalonia.Interactivity;
using System.Diagnostics;

using Microsoft.FSharp.Core;
using Microsoft.FSharp.Collections;
using System;
using System.Linq;

using OxyPlot;
using AdProgrammingGUIAvalonia.ViewModels;
using OxyPlot.Avalonia;

using System.Collections.Generic;
using static Microsoft.FSharp.Core.ByRefKinds;
using System.Drawing;

namespace AdProgrammingGUIAvalonia.Views;

public partial class MainView : UserControl
{
    MainViewModel mvm = new MainViewModel();
    List<DataPoint> points = new List<DataPoint>();

    enum ViewMode 
    {
        initial,
        trig
    }

    public MainView()
    {
        InitializeComponent();


        this.Loaded += (sender, args) =>
        {
            ResetEquationBox();
        };
    }

    bool showingErrors;

    public void ButtonClicked(object source, RoutedEventArgs args)
    {
        var button = (source as Button)!;
        switch (button.Name)
        {
            case "reset":
                {
                    ResetEquationBox();
                }
                break;
            case "compute":
                {
                    ComputeEquation();
                }
                break;
            case "toggleHistory":
                {
                    ToggleHistoryBox();
                }
                break;
            case "plusBtn":
                {
                    InsertSymbol("+");
                }
                break;
            case "minusBtn":
                {
                    InsertSymbol("-");
                }
                break;
            case "multiplyBtn":
                {
                    InsertSymbol("*");
                }
                break;
            case "divBtn":
                {
                    InsertSymbol("/");
                }
                break;
            case "floorDivBtn":
                {
                    InsertSymbol("//");
                }
                break;
            case "moduloBtn":
                {
                    InsertSymbol("%");
                }
                break;
            case "powerBtn":
                {
                    InsertSymbol("^");
                }
                break;
            case "parenthesisBtn":
                {
                    InsertSymbol("()");
                }
                break;
            case "assignmentBtn":
                {
                    InsertSymbol("=");
                }
                break;
            case "sinBtn":
                {
                    InsertSymbol("sin");
                }
                break;
            case "cosBtn":
                {
                    InsertSymbol("cos");
                }
                break;
            case "tanBtn":
                {
                    InsertSymbol("tan");
                }
                break;
            case "plot":
                {
                    CreatePlot();
                }
                break;
            case "initialView":
                {
                    ChangeViewMode(ViewMode.initial);
                }
                break;
            case "trigView":
                {
                    ChangeViewMode(ViewMode.trig);
                }
                break;
            case "resetPlot":
                {
                    ResetPlotEmptyInput();
                }
                break;
        }
    }

    private void CreatePlot()
    {
        ResetPlot();
        var plot = this.Find<Plot>("oPlot") as Plot;
        var yExprInputBox = this.Find<TextBox>("yExprInput") as TextBox;
        var intervalInputBox = this.Find<TextBox>("intervalInput") as TextBox;
        var xAxis = this.Find<LinearAxis>("plotXAxis") as LinearAxis;

        string xMinInput = "-10", xMaxInput = "10", yExprInput = "y=x", intervalInput = "0.1";
        double xMin = -10, xMax = 10;

        if (yExprInputBox != null)
        {
            yExprInput = yExprInputBox.Text ?? "y=x";
        }
        if (intervalInputBox != null)
        {
            intervalInput = intervalInputBox.Text ?? "0.1";
            if (intervalInput == "0")
            {
                intervalInput = "0.1";
            }
        }
        if (xAxis != null)
        {

            double xAxisLength = xAxis.InternalAxis.ActualMaximum - xAxis.InternalAxis.ActualMinimum;
            xMin = xAxis.InternalAxis.ActualMinimum - xAxisLength;
            xMax = xAxis.InternalAxis.ActualMaximum + xAxisLength;
            Debug.WriteLine("Min: " + xMin + ", Max: " + xMax);
        }

        if (plot != null)
        {
            CalculateY(xMin, xMax, yExprInput, Convert.ToDouble(intervalInput));
            //mvm.Points.Add(new DataPoint(Convert.ToDouble(xMinInput), Convert.ToDouble(xMaxInput)));            
            plot.Series[0].ItemsSource = mvm.Points;
            plot.InvalidatePlot(true);
        }
    }

    private void ChangeViewMode(ViewMode mode)
    {
        var plot = this.Find<Plot>("oPlot") as Plot;
        var xAxis = this.Find<LinearAxis>("plotXAxis") as LinearAxis;
        var yAxis = this.Find<LinearAxis>("plotYAxis") as LinearAxis;

        if (xAxis != null)
        {
            if (mode == ViewMode.initial)
            {
                xAxis.Minimum = -40;
                xAxis.Maximum = 40;
            }
            else if (mode == ViewMode.trig)
            {
                xAxis.Minimum = -720;
                xAxis.Maximum = 720;
            }
        }
        if (yAxis != null)
        {
            if (mode == ViewMode.initial)
            {
                yAxis.Minimum = -20;
                yAxis.Maximum = 20;
            } 
            else if (mode == ViewMode.trig)
            {
                yAxis.Minimum = -2;
                yAxis.Maximum = 2;
            }
        }
        if (plot != null)
        {
            plot.ResetAllAxes();
        }

    }

    private void CalculateY(double xMin, double xMax, string yExpr, double interval)
    {
        //double step = (xMax - xMin) / (points - 1);
        //step = Program.number.fltVal(Program.guiIntegration(((xMax - xMin) / (points - 1)).ToString()));
        Program.number y;
        int step = 1;
        double currentX = xMin;

        while (currentX <= xMax)
        {
            //Debug.WriteLine(currentX);
            try
            {
                if ((int)currentX == (double)currentX)
                {
                    Program.guiIntegration("x=" + currentX + ".0");
                }
                else
                {
                    Program.guiIntegration("x=" + currentX);
                }
                y = Program.guiIntegration("y=" + yExpr);
                //Debug.WriteLine("point: " + point.ToString() + ". x = " + currentX.ToString() + ", y = " + y.ToString());
                mvm.Points.Add(new DataPoint(Convert.ToDouble(currentX), Program.number.fltVal(y)));
            }
            catch (Exception ex)
            {
                Debug.WriteLine(ex.ToString());
            }

            currentX = (double)(xMin + interval * step);
            step++;
        }
    }

    private void ResetPlotEmptyInput()
    {
        ResetPlot();
        var plot = this.Find<Plot>("oPlot") as Plot;
        var xMinInputBox = this.Find<TextBox>("xMinInput") as TextBox;
        var xMaxInputBox = this.Find<TextBox>("xMaxInput") as TextBox;
        var yExprInputBox = this.Find<TextBox>("yExprInput") as TextBox;
        var intervalInputBox = this.Find<TextBox>("intervalInput") as TextBox;
        if (xMinInputBox != null)
        {
            xMinInputBox.Text = null;
        }
        if (xMaxInputBox != null)
        {
            xMaxInputBox.Text = null;
        }
        if (yExprInputBox != null)
        {
            yExprInputBox.Text = null;
        }
        if (intervalInputBox != null)
        {
            intervalInputBox.Text = null;
        }
    }

    private void ResetPlot()
    {
        var plot = this.Find<Plot>("oPlot") as Plot;
        if (plot != null)
        {
            mvm.ResetPoints();
            plot.Series[0].ItemsSource = mvm.Points;
            plot.InvalidatePlot(true);
        }
    }

    private void ResetEquationBox()
    {
        //Delete the text content of the equation box
        var equationBox = this.Find<TextBox>("equationInput") as TextBox;
        if (equationBox != null)
        {
            equationBox.Text = null;
            equationBox.Focus();
        }
    }

    private void ComputeEquation()
    {
        String inputText = GetEquationInput();
        if (inputText != null && inputText != string.Empty)
        {
            String result;
            bool isError;
            (result, isError) = GetResultFromFSharpParser(inputText);
            if (isError)
            {
                String newErrorItem = inputText + ": " + result;
                UpdateResultBox(result);
                UpdateErrorHistory(newErrorItem);
            }
            else
            {
                String newHistoryItem = inputText + " = " + result;
                UpdateResultBox(result);
                UpdateResultHistory(newHistoryItem);
            }
            ResetEquationBox();
        }
    }

    private void ToggleHistoryBox()
    {
        var toggleButton = this.Find<Button>("toggleHistory") as Button;
        var historyBox = this.Find<TextBox>("history") as TextBox;
        var errorBox = this.Find<TextBox>("error") as TextBox;
        if (toggleButton != null)
        {
            showingErrors = !showingErrors;
            toggleButton.Content = showingErrors ? "Show Result History" : "Show Error History";
            if (historyBox != null)
            {
                historyBox.IsVisible = !showingErrors;
            }
            if (errorBox != null)
            {
                errorBox.IsVisible = showingErrors;
            }
        }
    }

    private String GetEquationInput()
    {
        //First get a reference to the equation textbox
        //Then return the text it contains
        var equationBox = this.Find<TextBox>("equationInput") as TextBox;
        String inputText;
        if (equationBox != null)
        {
            inputText = equationBox.Text ?? "";
        }
        else
        {
            inputText = string.Empty;
        }
        return inputText;
    }

    private (String, bool) GetResultFromFSharpParser(String equation)
    {
        //First send the equation through to the F# parser
        //Then retrieve the result from the parser, and return it
        Program.number result;
        try
        {
            result = Program.guiIntegration(equation);
            return (result.ToString(), false);
        }
        catch (Exception ex)
        {
            return (ex.Message, true);
        }
    }

    private void UpdateResultBox(String result)
    {
        //Get reference to result textbox then update its contents to new result
        var resultBox = this.Find<TextBox>("result") as TextBox;
        if (resultBox != null)
        {
            resultBox.Text = result ?? "";
        }
    }

    private void UpdateErrorHistory(String newErrorHistoryItem)
    {
        var errorBox = this.Find<TextBox>("error") as TextBox;
        if (errorBox != null)
        {
            if (errorBox.Text != null)
            {
                errorBox.Text = errorBox.Text.Insert(0, newErrorHistoryItem + "\n");
            }
            else
            {
                errorBox.Text = newErrorHistoryItem;
            }
        }
    }

    private void UpdateResultHistory(String newHistoryItem)
    {
        //First get a reference to history textbox
        //Then update its contents by appending the most recent equation + result to the end
        var historyBox = this.Find<TextBox>("history") as TextBox;
        if (historyBox != null)
        {
            if (historyBox.Text != null)
            {
                historyBox.Text = historyBox.Text.Insert(0, newHistoryItem + "\n");
            }
            else
            {
                historyBox.Text = newHistoryItem;
            }
        }
    }

    private void InsertSymbol(string symbol)
    {
        var equationBox = this.Find<TextBox>("equationInput") as TextBox;
        int selection = 0;
        if (equationBox != null)
        {
            if (equationBox.Text != null)
            {
                selection = equationBox.SelectionStart;
                equationBox.Text = equationBox.Text.Insert(selection, symbol);
            }
            else
            {
                equationBox.Text = symbol;
            }
            selection += symbol.Length;
            if (symbol == "()")
            {
                selection--;
            }

            equationBox.Focus();
            equationBox.CaretIndex = selection;
        }
    }
}