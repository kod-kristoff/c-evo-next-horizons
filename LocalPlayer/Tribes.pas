{$INCLUDE Switches.inc}
unit Tribes;

interface

uses
  Protocol, ScreenTools, LazFileUtils, Classes, Graphics, SysUtils, Global,
  GraphicSet;

type
  TCityPicture = record
    xShield: Integer;
    yShield: Integer;
  end;

  TModelPicture = record
    HGr: TGraphicSet;
    pix: Integer;
    xShield: Integer;
    yShield: Integer;
  end;

  { TModelPictureInfo }

  TModelPictureInfo = record
    trix: Integer;
    mix: Integer;
    pix: Integer;
    Hash: Integer;
    GrName: ShortString;
    function GetCommandDataSize: Byte;
  end;

  TTribe = class
    symHGr: TGraphicSet;
    sympix: Integer;
    faceHGr: TGraphicSet;
    facepix: Integer;
    cHGr: TGraphicSet;
    cpix: Integer;
    // symbol and city graphics
    cAge: Integer;
    mixSlaves: Integer;
    Color: TColor;
    NumberName: Integer;
    CityPicture: array [0 .. 3] of TCityPicture;
    ModelPicture: array [-1 .. 256] of TModelPicture; // -1 is building site
    ModelName: array [-1 .. 256] of string;
    constructor Create(FileName: string);
    destructor Destroy; override;
    function GetCityName(I: Integer): string;
{$IFNDEF SCR} procedure SetCityName(I: Integer; NewName: string); {$ENDIF}
{$IFNDEF SCR} function TString(Template: string): string;
    function TPhrase(Item: string): string; {$ENDIF}
    procedure SetModelPicture(const Info: TModelPictureInfo; IsNew: Boolean);
    function ChooseModelPicture(var Picture: TModelPictureInfo;
      Code, Turn: Integer; ForceNew: Boolean): Boolean;
    procedure InitAge(Age: Integer);
  protected
    CityLine0: Integer;
    nCityLines: Integer;
    Name: array ['a' .. 'z'] of string;
    Script: TStringList;
  end;

var
  Tribe: array [0 .. nPl - 1] of TTribe;
  HGrStdUnits: TGraphicSet;

procedure Init;
procedure Done;
function CityName(Founder: Integer): string;
function ModelCode(const ModelInfo: TModelInfo): Integer;
procedure FindStdModelPicture(Code: Integer; var pix: Integer; var Name: string);
function GetTribeInfo(FileName: string; var Name: string; var Color: TColor): Boolean;
procedure FindPosition(HGr: TGraphicSet; X, Y, xmax, ymax: Integer; Mark: TColor;
  var xp, yp: Integer);


implementation

uses
  Directories;

type
  TChosenModelPictureInfo = record
    Hash: Integer;
    HGr: TGraphicSet;
    pix: Integer;
    ModelName: ShortString;
  end;

  TPictureList = array [0 .. 99999] of TChosenModelPictureInfo;

var
  StdUnitScript: TStringList;
  PictureList: ^TPictureList;
  nPictureList: Integer;

procedure Init;
begin
  StdUnitScript := TStringList.Create;
  StdUnitScript.LoadFromFile(LocalizedFilePath('Tribes' +
    DirectorySeparator + 'StdUnits.txt'));
  nPictureList := 0;
  PictureList := nil;
end;

procedure Done;
begin
  ReallocMem(PictureList, 0);
  FreeAndNil(StdUnitScript);
end;

function CityName(Founder: Integer): string;
begin
  if not GenerateNames then
    Result := Format('%d.%d', [Founder shr 12, Founder and $FFF])
  else
    Result := Tribe[Founder shr 12].GetCityName(Founder and $FFF);
end;

function ModelCode(const ModelInfo: TModelInfo): Integer;
begin
  with ModelInfo do
  begin
    case Kind of
      mkSelfDeveloped, mkEnemyDeveloped:
        case Domain of { age determination }
          dGround:
            if (Attack >= Defense * 4) or (Attack > 0) and
              (MaxUpgrade < 10) and
              (Cap and (1 shl (mcArtillery - mcFirstNonCap)) <> 0) then
            begin
              Result := 170;
              if MaxUpgrade >= 12 then
                Inc(Result, 3)
              else if (MaxUpgrade >= 10) or (Weight > 7) then
                Inc(Result, 2)
              else if MaxUpgrade >= 4 then
                Inc(Result, 1);
            end
            else
            begin
              Result := 100;
              if MaxUpgrade >= 12 then
                Inc(Result, 6)
              else if (MaxUpgrade >= 10) or (Weight > 7) then
                Inc(Result, 5)
              else if MaxUpgrade >= 6 then
                Inc(Result, 4)
              else if MaxUpgrade >= 4 then
                Inc(Result, 3)
              else if MaxUpgrade >= 2 then
                Inc(Result, 2)
              else if MaxUpgrade >= 1 then
                Inc(Result, 1);
              if Speed >= 250 then
                if (Result >= 105) and (Attack <= Defense) then
                  Result := 110
                else
                  Inc(Result, 30);
            end;
          dSea:
          begin
            Result := 200;
            if MaxUpgrade >= 8 then
              Inc(Result, 3)
            else if MaxUpgrade >= 6 then
              Inc(Result, 2)
            else if MaxUpgrade >= 3 then
              Inc(Result, 1);
            if Cap and (1 shl (mcSub - mcFirstNonCap)) <> 0 then
              Result := 240
            else if ATrans_Fuel > 0 then
              Result := 220
            else if (Result >= 202) and (Attack = 0) and (TTrans > 0) then
              Result := 210;
          end;
          dAir:
          begin
            Result := 300;
            if (Bombs > 0) or (TTrans > 0) then
              Inc(Result, 10);
            if Speed > 850 then
              Inc(Result, 1);
          end;
        end;
      mkSpecial_TownGuard:
        Result := 41;
      mkSpecial_Boat:
        Result := 64;
      mkSpecial_SubCabin:
        Result := 71;
      mkSpecial_Glider:
        Result := 73;
      mkSlaves:
        Result := 74;
      mkSettler:
        if Speed > 150 then
          Result := 11
        else
          Result := 10;
      mkDiplomat:
        Result := 21;
      mkCaravan:
        Result := 30;
    end;
  end;
end;

var
  Input: string;

function Get: string;
var
  P: Integer;
begin
  while (Input <> '') and ((Input[1] = ' ') or (Input[1] = #9)) do
    Delete(Input, 1, 1);
  P := Pos(',', Input);
  if P = 0 then
    P := Length(Input) + 1;
  Result := Copy(Input, 1, P - 1);
  Delete(Input, 1, P);
end;

function GetNum: Integer;
var
  I: Integer;
begin
  Val(Get, Result, I);
  if I <> 0 then
    Result := 0;
end;

procedure FindStdModelPicture(Code: Integer; var pix: Integer; var Name: string);
var
  I: Integer;
begin
  for I := 0 to StdUnitScript.Count - 1 do
  begin // look through StdUnits
    Input := StdUnitScript[I];
    pix := GetNum;
    if Code = GetNum then
    begin
      Name := Get;
      Exit;
    end;
  end;
  pix := -1;
end;

function GetTribeInfo(FileName: string; var Name: string;
  var Color: TColor): Boolean;
var
  Found: Integer;
  TribeScript: TextFile;
begin
  Name := '';
  Color := $FFFFFF;
  Found := 0;
  AssignFile(TribeScript, LocalizedFilePath('Tribes' + DirectorySeparator +
    FileName + CevoTribeExt));
  Reset(TribeScript);
  while not EOF(TribeScript) do
  begin
    ReadLn(TribeScript, Input);
    if Copy(Input, 1, 7) = '#CHOOSE' then
    begin
      Name := Copy(Input, 9, 255);
      Found := Found or 1;
      if Found = 3 then
        Break;
    end
    else if Copy(Input, 1, 6) = '#COLOR' then
    begin
      Color := HexStringToColor(Copy(Input, 7, 255));
      Found := Found or 2;
      if Found = 3 then
        Break;
    end;
  end;
  CloseFile(TribeScript);
  Result := Found = 3;
end;

{ TModelPictureInfo }

function TModelPictureInfo.GetCommandDataSize: Byte;
begin
  Result := SizeOf(trix) + SizeOf(mix) + SizeOf(pix) + SizeOf(Hash) + 1 +
    Length(GrName);
end;

constructor TTribe.Create(FileName: string);
var
  Line: Integer;
  Variant: Char;
  Item: string;
begin
  inherited Create;
  for Variant := 'a' to 'z' do
    Name[Variant] := '';
  Script := TStringList.Create;
  Script.LoadFromFile(FileName);
  CityLine0 := 0;
  nCityLines := 0;
  for Line := 0 to Script.Count - 1 do
  begin
    Input := Script[Line];
    if (CityLine0 > 0) and (nCityLines = 0) and
      ((Input = '') or (Input[1] = '#')) then
      nCityLines := Line - CityLine0;
    if (Length(Input) >= 3) and (Input[1] = '#') and
      (Input[2] in ['a' .. 'z']) and (Input[3] = ' ') then
      Name[Input[2]] := Copy(Input, 4, 255)
    else if Copy(Input, 1, 6) = '#COLOR' then
      Color := HexStringToColor(Copy(Input, 7, 255))
    else if Copy(Input, 1, 7) = '#CITIES' then
      CityLine0 := Line + 1
    else if Copy(Input, 1, 8) = '#SYMBOLS' then
    begin
      Delete(Input, 1, 9);
      Item := Get;
      sympix := GetNum;
      symHGr := LoadGraphicSet(Item + '.png');
    end;
  end;
  FillChar(ModelPicture, SizeOf(ModelPicture), 0);
  NumberName := -1;
  cAge := -1;
  mixSlaves := -1;
end;

destructor TTribe.Destroy;
begin
  FreeAndNil(Script);
  inherited;
end;

procedure FindPosition(HGr: TGraphicSet; X, Y, xmax, ymax: Integer; Mark: TColor;
  var xp, yp: Integer);
begin
  xp := 0;
  while (xp < xmax) and (HGr.Data.Canvas.Pixels[X + 1 + xp, Y] <> Mark) do
    Inc(xp);
  yp := 0;
  while (yp < ymax) and (HGr.Data.Canvas.Pixels[X, Y + 1 + yp] <> Mark) do
    Inc(yp);
end;

function TTribe.GetCityName(I: Integer): string;
begin
  Result := '';
  if nCityLines > I then
  begin
    Result := Script[CityLine0 + I];
    while (Result <> '') and ((Result[1] = ' ') or (Result[1] = #9)) do
      Delete(Result, 1, 1);
  end
{$IFNDEF SCR}
  else
    Result := Format(TPhrase('GENCITY'), [I + 1]);
{$ENDIF}
end;

{$IFNDEF SCR}
procedure TTribe.SetCityName(I: Integer; NewName: string);
begin
  while nCityLines <= I do
  begin
    Script.Insert(CityLine0 + nCityLines, Format(TPhrase('GENCITY'),
      [nCityLines + 1]));
    Inc(nCityLines);
  end;
  Script[CityLine0 + I] := NewName;
end;

function TTribe.TString(Template: string): string;
var
  P: Integer;
  Variant: Char;
  CaseUp: Boolean;
begin
  repeat
    P := Pos('#', Template);
    if (P = 0) or (P = Length(Template)) then
      Break;
    Variant := Template[P + 1];
    CaseUp := Variant in ['A' .. 'Z'];
    if CaseUp then
      Inc(Variant, 32);
    Delete(Template, P, 2);
    if Variant in ['a' .. 'z'] then
    begin
      if NumberName < 0 then
        Insert(Name[Variant], Template, P)
      else
        Insert(Format('P%d', [NumberName]), Template, P);
      if CaseUp and (Length(Template) >= P) and
        (Template[P] in ['a' .. 'z', #$E0 .. #$FF]) then
        Dec(Template[P], 32);
    end
  until False;
  Result := Template;
end;

function TTribe.TPhrase(Item: string): string;
begin
  Result := TString(Phrases.Lookup(Item));
end;

{$ENDIF}

procedure TTribe.InitAge(Age: Integer);
type
  TLine = array [0 .. 649, 0 .. 2] of Byte;
var
  I, X, Gray: Integer;
  Item: string;
begin
  if Age = cAge then
    Exit;
  cAge := Age;
  with Script do
  begin
    I := 0;
    while (I < Count) and (Copy(Strings[I], 1, 6) <>
        '#AGE' + char(48 + Age) + ' ') do
      Inc(I);
    if I < Count then
    begin
      Input := Strings[I];
      system.Delete(Input, 1, 6);
      Item := Get;
      cpix := GetNum;
      // init city graphics
      if Age < 2 then
      begin
        if CompareText(Item, 'stdcities') = 0 then
          case cpix of
            3: cpix := 0;
            6: begin
              cpix := 0;
              Item := 'Nation2';
            end;
          end;
        cHGr := LoadGraphicSet(Item + '.png');
        for X := 0 to 3 do
          with CityPicture[X] do begin
            FindPosition(cHGr, X * 65, cpix * 49, 63, 47, $00FFFF,
              xShield, yShield);
            // FindPosition(cHGr,x*65,cpix*49,$FFFFFF,xf,yf);
          end;
      end
      else
        cHGr := nil;

{$IFNDEF SCR}
      Get;
      GetNum;
      Item := Get;
      if Item = '' then
        faceHGr := nil
      else
      begin
        faceHGr := LoadGraphicSet(Item + '.png');
        facepix := GetNum;
        if faceHGr.Data.Canvas.Pixels[facepix mod 10 * 65,
          facepix div 10 * 49 + 48] = $00FFFF then
        begin // generate shield picture
          faceHGr.Data.Canvas.Pixels[facepix mod 10 * 65,
            facepix div 10 * 49 + 48] := $000000;
          Gray := $B8B8B8;
          ImageOp_BCC(faceHGr.Data, Templates.Data,
            facepix mod 10 * 65 + 1, facepix div 10 * 49 + 1, 1, 25, 64, 48,
            Gray, Color);
        end;
      end;
{$ENDIF}
    end;
  end;
end;

procedure TTribe.SetModelPicture(const Info: TModelPictureInfo; IsNew: Boolean);
var
  I: Integer;
  ok: Boolean;
begin
  with Info do
  begin
    if not IsNew then
    begin
      I := nPictureList - 1;
      while (I >= 0) and (PictureList[I].Hash <> Info.Hash) do
        Dec(I);
      Assert(I >= 0);
      Assert(PictureList[I].HGr = LoadGraphicSet(GrName));
      Assert(PictureList[I].pix = pix);
      ModelPicture[mix].HGr := PictureList[I].HGr;
      ModelPicture[mix].pix := PictureList[I].pix;
      ModelName[mix] := PictureList[I].ModelName;
    end
    else
    begin
      with ModelPicture[mix] do
      begin
        HGr := LoadGraphicSet(GrName);
        pix := Info.pix;
        Inc(HGr.pixUsed[pix]);
      end;
      ModelName[mix] := '';

      // read model name from tribe script
      ok := False;
      for I := 0 to Script.Count - 1 do
      begin
        Input := Script[I];
        if Input = '#UNITS ' + ExtractFileNameOnly(GrName) then
          ok := True
        else if (Input <> '') and (Input[1] = '#') then
          ok := False
        else if ok and (GetNum = pix) then
        begin
          Get;
          ModelName[mix] := Get;
        end;
      end;

      if ModelName[mix] = '' then
      begin // read model name from StdUnits.txt
        for I := 0 to StdUnitScript.Count - 1 do
        begin
          Input := StdUnitScript[I];
          if GetNum = pix then
          begin
            Get;
            ModelName[mix] := Get;
          end;
        end;
      end;

      if Hash <> 0 then
      begin
        if nPictureList = 0 then
          ReallocMem(PictureList, 64 * SizeOf(TChosenModelPictureInfo))
        else if (nPictureList >= 64) and (nPictureList and
          (nPictureList - 1) = 0) then
          ReallocMem(PictureList,
            nPictureList * (2 * SizeOf(TChosenModelPictureInfo)));
        PictureList[nPictureList].Hash := Info.Hash;
        PictureList[nPictureList].HGr := ModelPicture[mix].HGr;
        PictureList[nPictureList].pix := Info.pix;
        PictureList[nPictureList].ModelName := ModelName[mix];
        Inc(nPictureList);
      end;
    end;

    with ModelPicture[mix] do
      FindPosition(HGr, pix mod 10 * 65, pix div 10 * 49, 63, 47, $FFFFFF,
        xShield, yShield);
  end;
end;

function TTribe.ChooseModelPicture(var Picture: TModelPictureInfo;
  Code, Turn: Integer; ForceNew: Boolean): Boolean;
var
  I: Integer;
  Cnt: Integer;
  HGr: TGraphicSet;
  Used: Integer;
  LeastUsed: Integer;
  TestPic: TModelPictureInfo;
  ok: Boolean;

  procedure Check;
  begin
    TestPic.pix := GetNum;
    if Code = GetNum then
    begin
      if ForceNew or (not Assigned(HGr)) then
        Used := 0
      else
      begin
        Used := 4 * HGr.pixUsed[TestPic.pix];
        if HGr = HGrStdUnits then
          Inc(Used, 2); // prefer units not from StdUnits
      end;
      if Used < LeastUsed then
      begin
        Cnt := 0;
        LeastUsed := Used;
      end;
      if Used = LeastUsed then
      begin
        Inc(Cnt);
        if Turn mod Cnt = 0 then
          Picture := TestPic;
      end;
    end;
  end;

begin
  // look for identical model to assign same picture again
  if not ForceNew and (Picture.Hash > 0) then
  begin
    for I := 0 to nPictureList - 1 do
      if PictureList[I].Hash = Picture.Hash then
      begin
        Picture.GrName := PictureList[I].HGr.Name;
        Picture.pix := PictureList[I].pix;
        Result := False;
        Exit;
      end;
  end;

  Picture.pix := 0;
  TestPic := Picture;
  LeastUsed := MaxInt;

  TestPic.GrName := 'StdUnits.png';
  HGr := HGrStdUnits;
  for I := 0 to StdUnitScript.Count - 1 do
  begin // look through StdUnits
    Input := StdUnitScript[I];
    Check;
  end;

  ok := False;
  for I := 0 to Script.Count - 1 do
  begin // look through units defined in tribe script
    Input := Script[I];
    if Copy(Input, 1, 6) = '#UNITS' then
    begin
      ok := True;
      TestPic.GrName := Copy(Input, 8, 255) + '.png';
      HGr := GrExt.SearchByName(TestPic.GrName);
    end
    else if (Input <> '') and (Input[1] = '#') then
      ok := False
    else if ok then
      Check;
  end;
  Result := True;
end;

end.
