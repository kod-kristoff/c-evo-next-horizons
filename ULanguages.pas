unit ULanguages;

{$mode delphi}

interface

uses
  Classes, SysUtils, fgl;

type
  TLanguage = class
    ShortName: string;
    FullName: string;
    Author: string;
  end;

  { TLanguages }

  TLanguages = class(TFPGObjectList<TLanguage>)
    procedure AddItem(const ShortName, FullName: string);
    procedure LoadToStrings(Strings: TStrings);
    function Search(ShortName: string): Integer;
  end;

var
  Languages: TLanguages;


implementation

{ TLanguages }

procedure TLanguages.AddItem(const ShortName, FullName: string);
var
  Language: TLanguage;
begin
  Language := TLanguage.Create;
  Language.ShortName := ShortName;
  Language.FullName := FullName;
  Add(Language);
end;

procedure TLanguages.LoadToStrings(Strings: TStrings);
var
  I: Integer;
begin
  Strings.Clear;
  for I := 0 to Count - 1 do
    Strings.Add(Items[I].FullName);
end;

function TLanguages.Search(ShortName: string): Integer;
var
  I: Integer;
begin
  I := 0;
  while (I < Count) and (Items[I].ShortName <> ShortName) do Inc(I);
  if I < Count then Result := I
    else Result := -1;
end;

initialization

Languages := TLanguages.Create;
Languages.AddItem('', 'System');
Languages.AddItem('cs', 'Czech');
Languages.AddItem('de', 'German');
Languages.AddItem('en', 'English');
Languages.AddItem('it', 'Italian');
Languages.AddItem('ru', 'Russian');
Languages.AddItem('zh-Hant', 'Traditional Chinese');
Languages.AddItem('zh-Hans', 'Simplified Chinese');

finalization

FreeAndNil(Languages);

end.

