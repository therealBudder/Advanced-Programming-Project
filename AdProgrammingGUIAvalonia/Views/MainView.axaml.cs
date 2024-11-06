using Avalonia;
using Avalonia.Controls;
using Avalonia.Interactivity;
using System.Diagnostics;

using Microsoft.FSharp.Core;
using Microsoft.FSharp.Collections;
using System;
using System.Linq;

using OxyPlot;

namespace AdProgrammingGUIAvalonia.Views;

public partial class MainView : UserControl
{
    public MainView()
    {
        InitializeComponent();
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
        }
    }

    private void ResetEquationBox()
    {
        //Delete the text content of the equation box
        var equationBox = this.Find<TextBox>("equationInput") as TextBox;
        if (equationBox != null)
        {
            equationBox.Text = string.Empty;
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
            toggleButton.Content = showingErrors ? "Show History" : "Show Errors";
            historyBox.IsVisible = !showingErrors;
            errorBox.IsVisible = showingErrors;
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
        double result;
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
        String errorHistoryText;
        String updatedErrorHistoryText;
        if (errorBox != null)
        {
            errorHistoryText = errorBox.Text ?? string.Empty;
            updatedErrorHistoryText = errorHistoryText + newErrorHistoryItem + "\n";
            errorBox.Text = updatedErrorHistoryText;
        }
    }

    private void UpdateResultHistory(String newHistoryItem)
    {
        //First get a reference to history textbox
        //Then update its contents by appending the most recent equation + result to the end
        var historyBox = this.Find<TextBox>("history") as TextBox;
        String historyText;
        String updatedHistoryText;
        if (historyBox != null)
        {
            historyText = historyBox.Text ?? string.Empty;
            updatedHistoryText = historyText + newHistoryItem + "\n";
            historyBox.Text = updatedHistoryText;
        }
    }
}
