object CityTypeDlg: TCityTypeDlg
  Left = 194
  Top = 38
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 395
  ClientWidth = 452
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
  OnMouseDown = PaintBox1MouseDown
  OnMouseUp = PaintBox1MouseUp
  OnPaint = FormPaint
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object CloseBtn: TButtonB
    Left = 414
    Top = 6
    Width = 25
    Height = 25
    Down = False
    Permanent = False
    OnClick = CloseBtnClick
    ButtonIndex = 0
  end
  object DeleteBtn: TButtonB
    Left = 410
    Top = 172
    Width = 25
    Height = 25
    Down = False
    Permanent = False
    OnClick = DeleteBtnClick
    ButtonIndex = 21
  end
end
