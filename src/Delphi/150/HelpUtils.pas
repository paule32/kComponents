unit HelpUtils;

interface

function StripNonAlphaNumeric(const AValue: string): string;

implementation

function StripNonAlphaNumeric(const AValue: string): string;
var
  SrcPtr, DestPtr: PChar;
begin
  SrcPtr := PChar(AValue);
  SetLength(Result, Length(AValue));
  DestPtr := PChar(Result);
  while SrcPtr[0] <> #0 do begin
    if SrcPtr[0] in ['a'..'z', 'A'..'Z', '0'..'9'] then begin
      DestPtr[0] := SrcPtr[0];
      Inc(DestPtr);
    end;
    Inc(SrcPtr);
  end;
  SetLength(Result, DestPtr - PChar(Result));
end;

end.
 