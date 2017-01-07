object CityDlg: TCityDlg
  Left = 1186
  Top = 183
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 480
  ClientWidth = 640
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
  OnKeyDown = FormKeyDown
  OnMouseDown = FormMouseDown
  OnPaint = FormPaint
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object CloseBtn: TButtonA
    Left = 335
    Top = 447
    Width = 100
    Height = 25
    Down = False
    Permanent = False
    OnClick = CloseBtnClick
    Caption = ''
  end
  object PrevCityBtn: TButtonC
    Left = 270
    Top = 6
    Width = 12
    Height = 12
    Down = False
    Permanent = False
    OnClick = PrevCityBtnClick
    ButtonIndex = 1
  end
  object NextCityBtn: TButtonC
    Left = 270
    Top = 18
    Width = 12
    Height = 12
    Down = False
    Permanent = False
    OnClick = NextCityBtnClick
    ButtonIndex = 0
  end
  object PageUpBtn: TButtonC
    Left = 245
    Top = 390
    Width = 12
    Height = 12
    Down = False
    Permanent = False
    OnClick = PageUpBtnClick
    ButtonIndex = 1
  end
  object PageDownBtn: TButtonC
    Left = 245
    Top = 402
    Width = 12
    Height = 12
    Down = False
    Permanent = False
    OnClick = PageDownBtnClick
    ButtonIndex = 0
  end
  object BuyBtn: TButtonC
    Left = 495
    Top = 317
    Width = 12
    Height = 12
    Down = False
    Permanent = False
    OnClick = BuyClick
    ButtonIndex = 5
  end
  object ProjectArea: TArea
    Left = 335
    Top = 280
    Width = 56
    Height = 40
  end
  object PrimacyArea: TArea
    Left = 11
    Top = 271
    Width = 57
    Height = 57
  end
  object Imp2Area: TArea
    Left = 192
    Top = 352
    Width = 56
    Height = 40
  end
  object Imp4Area: TArea
    Left = 120
    Top = 408
    Width = 56
    Height = 40
  end
  object Imp0Area: TArea
    Left = 48
    Top = 352
    Width = 56
    Height = 40
  end
  object Imp3Area: TArea
    Left = 48
    Top = 408
    Width = 56
    Height = 40
  end
  object Imp5Area: TArea
    Left = 192
    Top = 408
    Width = 56
    Height = 40
  end
  object Imp1Area: TArea
    Left = 120
    Top = 352
    Width = 56
    Height = 40
  end
  object Pop0Area: TArea
    Left = 13
    Top = 49
    Width = 278
    Height = 26
  end
  object Pop1Area: TArea
    Left = 303
    Top = 30
    Width = 82
    Height = 41
  end
  object SupportArea: TArea
    Left = 271
    Top = 415
    Width = 64
    Height = 18
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 50
    OnTimer = Timer1Timer
    Left = 8
    Top = 8
  end
end
