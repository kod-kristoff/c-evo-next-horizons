object MessgDlg: TMessgDlg
  Left = 493
  Top = 431
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'C-evo'
  ClientHeight = 134
  ClientWidth = 418
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  OnPaint = FormPaint
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object Button1: TButtonA
    Left = 101
    Top = 104
    Width = 100
    Height = 25
    Down = False
    Permanent = False
    OnClick = Button1Click
    Caption = ''
  end
  object Button2: TButtonA
    Left = 217
    Top = 104
    Width = 100
    Height = 25
    Down = False
    Permanent = False
    OnClick = Button2Click
    Caption = ''
  end
end
