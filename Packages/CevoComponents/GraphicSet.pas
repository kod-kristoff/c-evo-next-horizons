unit GraphicSet;

interface

uses
  Classes, SysUtils, Graphics, Generics.Collections, LCLType, DOM,
  XMLRead, XMLWrite, XML;

type
  TGraphicSet = class;

  { TGraphicSetItem }

  TGraphicSetItem = class
  private
    function GetBoundsRect: TRect;
    procedure SetBoundsRect(AValue: TRect);
  public
    Name: string;
    Left: Integer;
    Top: Integer;
    Width: Integer;
    Height: Integer;
    GraphicSet: TGraphicSet;
    procedure DrawTo(Canvas: TCanvas; Pos: TPoint);
    procedure LoadFromNode(Node: TDOMNode);
    procedure SaveToNode(Node: TDOMNode);
    property BoundsRect: TRect read GetBoundsRect write SetBoundsRect;
  end;

  { TGraphicSetItems }

  TGraphicSetItems = class(TObjectList<TGraphicSetItem>)
    GraphicSet: TGraphicSet;
    function SearchByName(Name: string): TGraphicSetItem;
    function AddNew(Name: string): TGraphicSetItem;
    procedure LoadFromNode(Node: TDOMNode);
    procedure SaveToNode(Node: TDOMNode);
  end;

  { TGraphicSet }

  TGraphicSet = class
    Name: string;
    Data: TBitmap;
    Mask: TBitmap;
    pixUsed: array of Byte;
    Items: TGraphicSetItems;
    procedure ResetPixUsed;
    function GetItem(ItemName: string): TGraphicSetItem;
    procedure LoadFromFile(FileName: string);
    procedure SaveToFile(FileName: string);
    constructor Create;
    destructor Destroy; override;
  end;

  TGraphicSetClass = class of TGraphicSet;

  { TGraphicSets }

  TGraphicSets = class(TObjectList<TGraphicSet>)
    function SearchByName(Name: string): TGraphicSet;
    function AddNew(Name: string): TGraphicSet;
    procedure ResetPixUsed;
  end;

const
  GraphicSetFileRootNode = 'GraphicSet';
  GraphicSetFileExt = '.grs';


implementation

resourcestring
  SWrongFileFormat = 'Wrong file format.';
  SGraphicItemNotFound = 'Graphic item %s not found in graphic set %s.';

{ TGraphicSetItem }

function TGraphicSetItem.GetBoundsRect: TRect;
begin
  Result := Bounds(Left, Top, Width, Height);
end;

procedure TGraphicSetItem.SetBoundsRect(AValue: TRect);
begin
  Left := AValue.Left;
  Top := AValue.Top;
  Width := AValue.Width;
  Height := AValue.Height;
end;

procedure TGraphicSetItem.DrawTo(Canvas: TCanvas; Pos: TPoint);
begin
{  BitBltCanvas(Canvas, Pos.X, Pos.Y, BoundsRect.Width, BoundsRect.Height,
    GraphicSet.Mask.Canvas, BoundsRect.Left, BoundsRect.Top, SRCAND);
  BitBltCanvas(Canvas, Pos.X, Pos.Y, BoundsRect.Width, BoundsRect.Height,
    GraphicSet.Data.Canvas, BoundsRect.Left, BoundsRect.Top, SRCPAINT);
}
end;

procedure TGraphicSetItem.LoadFromNode(Node: TDOMNode);
begin
  Name := ReadString(Node, 'Name', Name);
  Left := ReadInteger(Node, 'Left', Left);
  Top := ReadInteger(Node, 'Top', Top);
  Width := ReadInteger(Node, 'Width', Width);
  Height := ReadInteger(Node, 'Height', Height);
end;

procedure TGraphicSetItem.SaveToNode(Node: TDOMNode);
begin
  WriteString(Node, 'Name', Name);
  WriteInteger(Node, 'Left', Left);
  WriteInteger(Node, 'Top', Top);
  WriteInteger(Node, 'Width', Width);
  WriteInteger(Node, 'Height', Height);
end;

{ TGraphicSetItems }

function TGraphicSetItems.SearchByName(Name: string): TGraphicSetItem;
var
  I: Integer;
begin
  I := 0;
  while (I < Count) and (Items[I].Name <> Name) do Inc(I);
  if I < Count then Result := Items[I]
    else Result := nil;
end;

function TGraphicSetItems.AddNew(Name: string): TGraphicSetItem;
begin
  Result := TGraphicSetItem.Create;
  Result.Name := Name;
  Add(Result);
end;

procedure TGraphicSetItems.LoadFromNode(Node: TDOMNode);
var
  Node2: TDOMNode;
  NewItem: TGraphicSetItem;
begin
  Count := 0;
  Node2 := Node.FirstChild;
  while Assigned(Node2) and (Node2.NodeName = 'Item') do begin
    NewItem := TGraphicSetItem.Create;
    NewItem.GraphicSet := GraphicSet;
    NewItem.LoadFromNode(Node2);
    Add(NewItem);
    Node2 := Node2.NextSibling;
  end;
end;

procedure TGraphicSetItems.SaveToNode(Node: TDOMNode);
var
  I: Integer;
  NewNode: TDOMNode;
begin
  for I := 0 to Count - 1 do begin;
    NewNode := Node.OwnerDocument.CreateElement('Item');
    Node.AppendChild(NewNode);
    Items[I].SaveToNode(NewNode);
  end;
end;

{ TGraphicSet }

procedure TGraphicSet.ResetPixUsed;
begin
  SetLength(pixUsed, Data.Height div 49 * 10);
  if Length(pixUsed) > 0 then
    FillChar(pixUsed[0], Length(pixUsed), 0);
end;

function TGraphicSet.GetItem(ItemName: string): TGraphicSetItem;
begin
  Result := Items.SearchByName(ItemName);
  if not Assigned(Result) then
    raise Exception.Create(Format(SGraphicItemNotFound, [ItemName, Name]));
end;

procedure TGraphicSet.LoadFromFile(FileName: string);
var
  Doc: TXMLDocument;
  RootNode: TDOMNode;
  NewNode: TDOMNode;
begin
  ReadXMLFile(Doc, FileName);
  with Doc do
  try
    if DocumentElement.NodeName <> GraphicSetFileRootNode then
      raise Exception.Create(SWrongFileFormat);
    RootNode := Doc.DocumentElement;
    with RootNode do begin
      NewNode := FindNode('Items');
      if Assigned(NewNode) then
        Items.LoadFromNode(NewNode);
    end;
  finally
    FreeAndNil(Doc);
  end;
end;

procedure TGraphicSet.SaveToFile(FileName: string);
var
  NewNode: TDOMNode;
  Doc: TXMLDocument;
  RootNode: TDOMNode;
begin
  Doc := TXMLDocument.Create;
  with Doc do
  try
    RootNode := CreateElement(GraphicSetFileRootNode);
    AppendChild(RootNode);
    with RootNode do begin
      NewNode := OwnerDocument.CreateElement('Items');
      AppendChild(NewNode);
      Items.SaveToNode(NewNode);
    end;
    WriteXMLFile(Doc, FileName);
  finally
    FreeAndNil(Doc);
  end;
end;

constructor TGraphicSet.Create;
begin
  Data := TBitmap.Create;
  Data.PixelFormat := pf24bit;
  Mask := TBitmap.Create;
  Mask.PixelFormat := pf24bit;
  Items := TGraphicSetItems.Create;
  Items.GraphicSet := Self;
end;

destructor TGraphicSet.Destroy;
begin
  FreeAndNil(Items);
  FreeAndNil(Data);
  FreeAndNil(Mask);
  inherited;
end;

{ TGraphicSets }

function TGraphicSets.SearchByName(Name: string): TGraphicSet;
var
  I: Integer;
begin
  I := 0;
  while (I < Count) and (Items[I].Name <> Name) do Inc(I);
  if I < Count then Result := Items[I]
    else Result := nil;
end;

function TGraphicSets.AddNew(Name: string): TGraphicSet;
begin
  Result := TGraphicSet.Create;
  Result.Name := Name;
  Add(Result);
end;

procedure TGraphicSets.ResetPixUsed;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].ResetPixUsed;
end;

end.


