object HelpDlg: THelpDlg
  Left = 394
  Top = 180
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 479
  ClientWidth = 560
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
  OnMouseDown = PaintBox1MouseDown
  OnMouseMove = PaintBox1MouseMove
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 16
  object CloseBtn: TButtonB
    Left = 522
    Top = 6
    Width = 25
    Height = 25
    Down = False
    Permanent = False
    OnClick = CloseBtnClick
    ButtonIndex = 0
  end
  object BackBtn: TButtonB
    Left = 42
    Top = 6
    Width = 25
    Height = 25
    Down = False
    Permanent = False
    OnClick = BackBtnClick
    ButtonIndex = 6
  end
  object TopBtn: TButtonB
    Left = 13
    Top = 6
    Width = 25
    Height = 25
    Down = False
    Permanent = False
    OnClick = TopBtnClick
    ButtonIndex = 7
  end
  object SearchBtn: TButtonB
    Left = 493
    Top = 6
    Width = 25
    Height = 25
    Down = False
    Permanent = False
    OnClick = SearchBtnClick
    ButtonIndex = 18
  end
end
