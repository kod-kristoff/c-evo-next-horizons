object UnitStatDlg: TUnitStatDlg
  Left = 344
  Top = 213
  BorderStyle = bsNone
  ClientHeight = 326
  ClientWidth = 208
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object SwitchBtn: TButtonB
    Left = 12
    Top = 29
    Width = 25
    Height = 25
    Down = False
    Permanent = False
    OnClick = SwitchBtnClick
    ButtonIndex = 11
  end
  object CloseBtn: TButtonB
    Left = 177
    Top = 6
    Width = 25
    Height = 25
    Down = False
    Permanent = False
    OnClick = CloseBtnClick
    ButtonIndex = 0
  end
  object ConscriptsBtn: TButtonB
    Left = 43
    Top = 29
    Width = 25
    Height = 25
    Down = False
    Permanent = False
    OnClick = ConscriptsBtnClick
    ButtonIndex = 29
  end
  object HelpBtn: TButtonC
    Left = 178
    Top = 145
    Width = 12
    Height = 12
    Down = False
    Permanent = False
    OnClick = HelpBtnClick
    ButtonIndex = 5
  end
end
