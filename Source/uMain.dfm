object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'AI - Claude'
  ClientHeight = 543
  ClientWidth = 981
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object lbSystem: TLabel
    Left = 8
    Top = 18
    Width = 36
    Height = 15
    Caption = #49884#49828#53596
  end
  object lbQuestion: TLabel
    Left = 8
    Top = 47
    Width = 48
    Height = 15
    Caption = #51656#51032#45236#50857
  end
  object eImage: TImage
    Left = 608
    Top = 16
    Width = 369
    Height = 521
    Stretch = True
  end
  object eSystem: TEdit
    Left = 96
    Top = 15
    Width = 505
    Height = 23
    TabOrder = 0
  end
  object btnExecute: TButton
    Left = 444
    Top = 75
    Width = 75
    Height = 25
    Caption = #50689#49688#51613' '#48516#49437
    TabOrder = 1
    OnClick = btnExecuteClick
  end
  object eResult: TMemo
    Left = 8
    Top = 104
    Width = 593
    Height = 433
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 526
    Top = 75
    Width = 75
    Height = 25
    Caption = #52712#49548
    TabOrder = 3
  end
  object eQuestion: TEdit
    Left = 96
    Top = 44
    Width = 505
    Height = 23
    TabOrder = 4
  end
  object btnTextQuestion: TButton
    Left = 362
    Top = 75
    Width = 75
    Height = 25
    Caption = #53581#49828#53944' '#51656#51032
    TabOrder = 5
    OnClick = btnTextQuestionClick
  end
  object btnBugs: TButton
    Left = 96
    Top = 75
    Width = 75
    Height = 25
    Caption = #48260#44536
    TabOrder = 6
    OnClick = btnBugsClick
  end
  object btnComplete: TButton
    Left = 176
    Top = 75
    Width = 75
    Height = 25
    Caption = 'Complete'
    TabOrder = 7
    OnClick = btnCompleteClick
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '*.jpg'
    Title = #51060#48120#51648' '#49440#53469
    Left = 560
    Top = 112
  end
end
