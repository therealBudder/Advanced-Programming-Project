using Avalonia;
using Avalonia.Controls;
using Avalonia.Interactivity;
using System.Diagnostics;

using Microsoft.FSharp.Core;
using Microsoft.FSharp.Collections;
using System;
using System.Linq;

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
                    ResetEquationBox();
                }
                break;
            case "compute":
                {
                    ComputeEquation();
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
            String result = GetResultFromFSharpParser(inputText);
            String newHistoryItem = inputText + " = " + result;
            UpdateResultBox(result);
            UpdateHistory(newHistoryItem);
            ResetEquationBox();
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

    private String GetResultFromFSharpParser(String equation)
    {
        //First send the equation through to the F# parser
        //Then retrieve the result from the parser, and return it
        var result = Program.guiIntegration(equation);
        return result.ToString();
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

    private void UpdateHistory(String newHistoryItem)
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
