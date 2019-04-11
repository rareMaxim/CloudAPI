unit uParseDIRegEx;

interface
uses Classes, DIRegEx, SysUtils;

  type
    TRegExpRec = record
      Ind: integer;
      Len: integer;
      Match: string;
      SubStr: string;
      SubStrCount: integer;
      LastSubStr: integer;
      SubStrList: TStringList;
  end;

  type
    TLastRegExpRec = record
      Len: integer;
      Match: string;
  end;

  type
    TParsedArray = array of TRegExpRec;


  function  ParseByDI(InputString: string; MatchPattern: string; SubStrInd: integer = 0): TParsedArray;
  function  GetLastMatch(InputString: string; MatchPattern: string): TLastRegExpRec;
  function  ReplaseByDi2(const InputStr: String; const MatchPattern: String; const NewString: String): String;
  procedure FreeParsedArray(ParsedArray: TParsedArray);

implementation

uses inifiles, Dialogs, {System.AnsiStrings,} uConstMsg;

function ParseByDI(InputString: string; MatchPattern: string; SubStrInd: integer = 0): TParsedArray;
var
  rexp: TDIPerlRegEx16;
 // tempArray: TParsedArray;
  count: Integer;
  i: Integer;
begin
  rexp := TDIPerlRegEx16.Create(nil);
  count := 0;

  try
    rexp.SetSubjectStr(InputString);
    rexp.MatchPattern   := MatchPattern;
    rexp.Options        := [poCaptureSubstrings, poMark, poAutoStudy];
    rexp.CompileOptions := [coUtf8, coCaseLess, coDotAll];

//    if (pos('(?i)', MatchPattern) <> 0) or (pos('(?is)', MatchPattern) <> 0) or (pos('(?si)', MatchPattern) <> 0) then
//      rexp.CompileOptions := [coCaseLess];

    rexp.CompileOptions := rexp.CompileOptions + [coUtf8];

    rexp.CompiledRegExpArray;

    if not rexp.CompileMatchPattern then
      begin
        ShowMessage(constInCorrectRexExp + rexp.MatchPattern);
        exit;
      end;

    if rexp.Match(0) < 0 then
      begin
        //если нет совпадений
        SetLength(result, 1);
        result[count].Ind        := 0;
        result[count].Len        := 0;
        result[count].Match      := '';
        result[count].SubStr     := '';
//        Result := TempArray;
        exit;
      end;

    repeat// пока есть совпадения
      SetLength(result, count+1);
      result[count].Ind          := count;
      result[count].Len          := rexp.MatchedStrLength;
      result[count].Match        := rexp.MatchedStr;
      result[count].SubStr       := rexp.SubStr(SubStrInd);
      result[count].SubStrCount  := rexp.SubStrCount;

      result[count].SubStrList := TStringList.Create;

      for i := 0 to rexp.SubStrCount - 1 do
        result[count].SubStrList.Add(rexp.SubStr(i));


      result[count].LastSubStr := pred(rexp.SubStrCount);

      Inc(count);
    until
      rexp.MatchNext < 0;

  finally
    FreeAndNil(rexp);
  end;
end;

function GetLastMatch(InputString: string; MatchPattern: string): TLastRegExpRec;
var
  rexp: TDIPerlRegEx16;
  tempArray: TLastRegExpRec;
//  i: Integer;
begin
  tempArray.Len        := 0;
  tempArray.Match      := '';

  rexp := TDIPerlRegEx16.Create(nil);


  try
    rexp.SetSubjectStr(InputString);
    rexp.MatchPattern   := MatchPattern;
//    rexp.Options        := [];
    rexp.CompileOptions := [];
    rexp.MatchOptions   := [];
    rexp.Options        := [poCaptureSubstrings, poMark, poAutoStudy];

    if (pos('(?i)', MatchPattern) <> 0) or (pos('(?is)', MatchPattern) <> 0) or (pos('(?si)', MatchPattern) <> 0) then
      rexp.CompileOptions := [coCaseLess];

    rexp.CompileOptions := rexp.CompileOptions + [coUtf8];

    if not rexp.CompileMatchPattern then
      ShowMessage(constInCorrectRexExp + rexp.MatchPattern);

    if rexp.Match(0) < 0 then
      begin
        //если нет совпадений
        tempArray.Len        := 0;
        tempArray.Match      := '';
        Result := TempArray;
        exit;
      end;


    tempArray.Len   := rexp.SubStrLength(Pred(rexp.SubStrCount));
    tempArray.Match := rexp.SubStr(pred(rexp.SubStrCount));// берём самую последнюю

    Result := TempArray;
  finally
    FreeAndNil(rexp);

  end;
end;


function ReplaseByDi2(const InputStr: String; const MatchPattern: String;const NewString: String): String;
var
  RE: TDIPerlRegEx16;
  AOptions: TDIRegexCompileOptions;
  i: integer;
  res: String;
begin
  RE := TDIPerlRegEx16.Create(nil);
  try
    RE.SetSubjectStr(InputStr);
    AOptions := [coCaseLess, coUtf8]; //[coCaseLess, coMultiline, coDotAll];
    RE.CompileOptions := AOptions;

    if not RE.CompileMatchPatternStr(MatchPattern) then showMessage(constError + ' - RE.CompileMatchPatternStr');

    RE.FormatPattern := NewString;

    res := result;
    i := RE.Replace2(res);

    if i = 0 then
      Result := InputStr
    else
      result := res;

  finally
    RE.Free;
  end;
end;

procedure FreeParsedArray(ParsedArray: TParsedArray);
Var
 g: integer;
begin
  for g := 0 to High(ParsedArray) do
    ParsedArray[g].SubStrList.free;
end;

end.
