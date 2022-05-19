{$INCLUDE Switches.inc}
unit Diplomacy;

interface

uses Protocol;

function DipCommandToString(pSender, pTarget, Treaty, OppCommand,
  Command: Integer; const OppOffer, Offer: TOffer): string;

implementation

uses
  ScreenTools, Tribes, SysUtils;

function DipCommandToString;

  function PriceToString(P, Price: Integer): string;
  begin
    case Price and opMask of
      opChoose:
        Result := Phrases.Lookup('PRICE_CHOOSE');
      opCivilReport:
        Result := Tribe[P].TPhrase('PRICE_CIVIL');
      opMilReport:
        Result := Tribe[P].TPhrase('PRICE_MIL');
      opMap:
        Result := Tribe[P].TPhrase('PRICE_MAP');
      opTreaty:
        { if Price-opTreaty<Treaty then
          case Treaty of
          trPeace: Result:=Phrases.Lookup('FRENDTREATY_PEACE');
          trFriendlyContact: Result:=Phrases.Lookup('FRENDTREATY_FRIENDLY');
          trAlliance: Result:=Phrases.Lookup('FRENDTREATY_ALLIANCE');
          end
          else } Result := Phrases.Lookup('TREATY', Price - opTreaty);
      opShipParts:
        case Price shr 16 and $F of
          0:
            Result := Format(Phrases.Lookup('PRICE_SHIPCOMP'),
              [Price and $FFFF]);
          1:
            Result := Format(Phrases.Lookup('PRICE_SHIPPOW'),
              [Price and $FFFF]);
          2:
            Result := Format(Phrases.Lookup('PRICE_SHIPHAB'),
              [Price and $FFFF]);
        end;
      opMoney:
        Result := Format('%d%%c', [Price - opMoney]);
      opTribute:
        Result := Format(Phrases.Lookup('PRICE_TRIBUTE'), [Price - opTribute]);
      opTech:
        Result := Phrases.Lookup('ADVANCES', Price - opTech);
      opAllTech:
        Result := Tribe[P].TPhrase('PRICE_ALLTECH');
      opModel:
        Result := Tribe[P].ModelName[Price - opModel];
      opAllModel:
        Result := Tribe[P].TPhrase('PRICE_ALLMODEL');
      { opCity:
        Result:=Format(TPhrase('PRICE_CITY',P),[CityName(Price-opCity)]); }
    end
  end;

var
  I: Integer;
  sAdd, sDeliver, sCost: string;
  DoIntro: Boolean;
begin
  DoIntro := OppCommand = scDipStart;
  case Command of
    scDipCancelTreaty:
      begin
        case Treaty of
          trPeace:
            Result := Phrases.Lookup('FRCANCELTREATY_PEACE');
          trFriendlyContact:
            Result := Phrases.Lookup('FRCANCELTREATY_FRIENDLY');
          trAlliance:
            Result := Phrases.Lookup('FRCANCELTREATY_ALLIANCE');
        end;
        DoIntro := False;
      end;
    scDipNotice:
      Result := Phrases.Lookup('FRNOTICE');
    scDipAccept:
      begin
        if (OppOffer.nDeliver + OppOffer.nCost = 1) and
          (OppOffer.Price[0] and opMask = opTreaty) and
          (Integer(OppOffer.Price[0] - opTreaty) > Treaty) then
        // simple treaty offer
          { if OppOffer.Price[0]-opTreaty=trCeaseFire then
            Result:=Tribe[pTarget].TPhrase('FRACCEPTCEASEFIRE')
            else } Result := Tribe[pTarget].TPhrase('FRACCEPTTREATY')
        else if OppOffer.nDeliver = 0 then
          Result := Tribe[pSender].TPhrase('FRACCEPTDEMAND_STRONG')
        else if OppOffer.nCost = 0 then
          Result := Tribe[pSender].TPhrase('FRACCEPTPRESENT')
        else
          Result := Tribe[pSender].TPhrase('FRACCEPTOFFER');
      end;
    scDipBreak:
      begin
        Result := Tribe[pTarget].TPhrase('FRBREAK');
        DoIntro := False;
      end;
    scDipOffer:
      begin
        Result := '';
        if (OppCommand = scDipOffer) and
          ((OppOffer.nDeliver > 0) or (OppOffer.nCost > 0)) and
          (Offer.nCost + Offer.nDeliver <= 2) then
        begin // respond to made offer before making own one
          if (OppOffer.nDeliver + OppOffer.nCost = 1) and
            (OppOffer.Price[0] and opMask = opTreaty) and
            (Integer(OppOffer.Price[0] - opTreaty) > Treaty) then
          // simple treaty offer
            Result := Tribe[pSender].TPhrase('FRNOTACCEPTTREATY') + '\'
          else if OppOffer.nDeliver = 0 then
            Result := Tribe[pSender].TPhrase('FRNOTACCEPTDEMAND_STRONG') + '\'
          else if OppOffer.nCost = 0 then
            Result := Tribe[pSender].TPhrase('FRNOTACCEPTPRESENT') + '\';
        end;

        sDeliver := '';
        for I := 0 to Offer.nDeliver - 1 do
        begin
          sAdd := PriceToString(pSender, Offer.Price[I]);
          if I = 0 then
            sDeliver := sAdd
          else
            sDeliver := Format(Phrases.Lookup('PRICE_CONCAT'), [sDeliver, sAdd])
        end;
        sCost := '';
        for I := 0 to Offer.nCost - 1 do
        begin
          sAdd := PriceToString(pTarget, Offer.Price[Offer.nDeliver + I]);
          if I = 0 then
            sCost := sAdd
          else
            sCost := Format(Phrases.Lookup('PRICE_CONCAT'), [sCost, sAdd])
        end;

        if (Offer.nDeliver = 0) and (Offer.nCost = 0) then
        begin // no offer made
          if (OppCommand = scDipOffer) and
            ((OppOffer.nDeliver = 0) and (OppOffer.nCost = 0)) then
            Result := Tribe[pTarget].TPhrase('FRBYE')
          else
          begin
            if (Result = '') and (OppCommand = scDipOffer) and
              ((OppOffer.nDeliver > 0) or (OppOffer.nCost > 0)) then
            begin
              if (OppOffer.nDeliver = 1) and (OppOffer.Price[0] = opChoose) and
                not Phrases2FallenBackToEnglish then
                Result := Tribe[pSender].TString
                  (Phrases2.Lookup('FRNOTACCEPTANYOFFER')) + ' '
              else if (OppOffer.nCost = 1) and
                (OppOffer.Price[OppOffer.nDeliver] = opChoose) and not Phrases2FallenBackToEnglish
              then
                Result := Tribe[pSender].TString
                  (Phrases2.Lookup('FRNOTACCEPTANYWANT')) + ' '
              else
                Result := Tribe[pSender].TPhrase('FRNOTACCEPTOFFER') + ' ';
            end;
            Result := Result + Phrases.Lookup('FRDONE');
            DoIntro := False
          end
        end
        else if (Offer.nDeliver + Offer.nCost = 1) and
          (Offer.Price[0] and opMask = opTreaty) and
          (Integer(Offer.Price[0] - opTreaty) > Treaty) then
        // simple treaty offer
        begin
          case Offer.Price[0] - opTreaty of
            // trCeaseFire: result:=result+Tribe[pTarget].TPhrase('FRCEASEFIRE');
            trPeace:
              Result := Result + Tribe[pTarget].TPhrase('FRPEACE');
            trFriendlyContact:
              Result := Result + Tribe[pTarget].TPhrase('FRFRIENDLY');
            trAlliance:
              Result := Result + Tribe[pTarget].TPhrase('FRALLIANCE');
          end
        end
        else if Offer.nDeliver = 0 then // demand
        begin
          if (Treaty >= trFriendlyContact) and not Phrases2FallenBackToEnglish
          then
            Result := Result +
              Format(Tribe[pTarget].TString(Phrases2.Lookup('FRDEMAND_SOFT')
              ), [sCost])
          else
          begin
            Result := Result +
              Format(Tribe[pTarget].TPhrase('FRDEMAND_STRONG'), [sCost]);
            DoIntro := False
          end
        end
        else if Offer.nCost = 0 then // present
          Result := Result + Format(Tribe[pTarget].TPhrase('FRPRESENT'),
            [sDeliver])
        else if (Offer.nDeliver = 1) and (Offer.Price[0] = opChoose) then
          Result := Result + Format(Phrases.Lookup('FRDELCHOICE'), [sCost])
        else if (Offer.nCost = 1) and (Offer.Price[Offer.nDeliver] = opChoose)
        then
          Result := Result + Format(Phrases.Lookup('FRCOSTCHOICE'), [sDeliver])
        else
          Result := Result + Format(Phrases.Lookup('FROFFER'),
            [sDeliver, sCost]);
      end;
  end;
  if DoIntro then
    if Treaty < trPeace then
      Result := Tribe[pSender].TPhrase('FRSTART_NOTREATY') + ' ' + Result
    else
      Result := Tribe[pSender].TPhrase('FRSTART_PEACE') + ' ' + Result
end;

end.
