object NatStatDlg: TNatStatDlg
  Left = 192
  Top = 119
  BorderStyle = bsNone
  ClientHeight = 480
  ClientWidth = 400
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
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object ToggleBtn: TButtonB
    Left = 6
    Top = 6
    Width = 25
    Height = 25
    Down = False
    Permanent = False
    OnClick = ToggleBtnClick
    ButtonIndex = 28
  end
  object CloseBtn: TButtonB
    Left = 369
    Top = 6
    Width = 25
    Height = 25
    Down = False
    Permanent = False
    OnClick = CloseBtnClick
    ButtonIndex = 0
  end
  object ScrollUpBtn: TButtonC
    Left = 381
    Top = 278
    Width = 12
    Height = 12
    Down = False
    Permanent = False
    OnClick = ScrollUpBtnClick
    ButtonIndex = 1
  end
  object ScrollDownBtn: TButtonC
    Left = 381
    Top = 290
    Width = 12
    Height = 12
    Down = False
    Permanent = False
    OnClick = ScrollDownBtnClick
    ButtonIndex = 0
  end
  object ContactBtn: TEOTButton
    Left = 330
    Top = 45
    Width = 48
    Height = 48
    Down = False
    Permanent = False
    OnClick = DialogBtnClick
    ButtonIndex = 3
  end
  object TellAIBtn: TButtonC
    Left = 24
    Top = 450
    Width = 12
    Height = 12
    Down = False
    Permanent = False
    OnClick = TellAIBtnClick
    ButtonIndex = 0
  end
  object Popup: TPopupMenu
    Left = 16
    Top = 48
  end
end
