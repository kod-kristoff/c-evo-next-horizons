object NoTermDlg: TNoTermDlg
  Left = 249
  Top = 258
  BorderStyle = bsNone
  ClientHeight = 456
  ClientWidth = 422
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 16
  object QuitBtn: TButtonB
    Left = 384
    Top = 6
    Width = 25
    Height = 25
    Down = False
    Permanent = False
    OnClick = QuitBtnClick
    ButtonIndex = 0
  end
  object GoBtn: TButtonB
    Left = 30
    Top = 422
    Width = 25
    Height = 25
    Down = False
    Permanent = False
    OnClick = GoBtnClick
    ButtonIndex = 0
  end
end
