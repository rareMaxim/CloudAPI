unit uVictorine;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IniFiles;

Type
  TOnAnswer = procedure(Sender: TObject; Const Question, Answer: String) of object;
  TOnNotAnswer = procedure(Sender: TObject; Const Question: String) of object;

  TVictorine = Class(TComponent)
  private
    fListQuestion, fListAnswer: THashedStringList;
    FOnAnswer: TOnAnswer;
    FOnNotAnswer: TOnNotAnswer;
    function GetQuitz(Index: Integer): String;
  protected
    Class Function Split(Const Text: String; Delim: String = '|'): TArray<String>;
    Class Function StopTrap(Const Text: String): String;
  public

    constructor Create(AOwner: TComponent); overload; override;
    destructor Destroy; override;

    Procedure LoadFromFile(Const FileName: String);
    Procedure SaveToFile(Const FileName: String);

    Procedure Add(Const Question, Answer: string);
    Function Answer(Const Question: string): TArray<String>;
    Function Count: Integer;
    Function RandomQuitz: String;
    //
    property Quitz[Index: Integer]: String read GetQuitz;

  published

    property OnAnswer: TOnAnswer read FOnAnswer write FOnAnswer;
    property OnNotAnswer: TOnNotAnswer read FOnNotAnswer write FOnNotAnswer;
  End;

implementation

{ TVictorine }

procedure TVictorine.Add(const Question, Answer: string);
var
  Index: Integer;
begin
  Index := fListQuestion.IndexOf(Question);
  if Index > -1 then
  Begin
    fListAnswer[Index] := fListAnswer[Index] + '|' + Answer;
  End
  else
  Begin
    fListQuestion.Add(Question);
    fListAnswer.Add(Answer);
  End;
end;

function TVictorine.Answer(const Question: string): TArray<String>;
var
  Iterator: Integer;
  tmpQuestion: String;
begin
  SetLength(Result, 0);
  tmpQuestion := StopTrap(Question);
  Iterator := fListQuestion.IndexOf(tmpQuestion);
  if Iterator > -1 then
    Result := Split(fListAnswer[Iterator]);
  if Assigned(FOnAnswer) then
    for Iterator := Low(Result) to High(Result) do
      FOnAnswer(Self, Question, Result[Iterator]);
  if Assigned(OnNotAnswer) then
    if Length(Result) = 0 then
      OnNotAnswer(Self, Question);
end;

function TVictorine.Count: Integer;
begin
  Result := fListQuestion.Count;
end;

constructor TVictorine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fListQuestion := THashedStringList.Create;
  fListAnswer := THashedStringList.Create;
end;

destructor TVictorine.Destroy;
begin
  fListQuestion.Free;
  fListAnswer.Free;
  inherited;
end;

function TVictorine.GetQuitz(Index: Integer): String;
begin
  Result := fListQuestion[Index];
end;

procedure TVictorine.LoadFromFile(const FileName: String);
var
  aFile: THashedStringList;
  I: Integer;
begin
  aFile := THashedStringList.Create;
  try
    aFile.LoadFromFile(FileName);
    for I := 0 to aFile.Count - 1 do
    Begin
      fListQuestion.Add(Self.Split(aFile[I])[0]);
      fListAnswer.Add(Copy(aFile[I], Pos('|', aFile[I]) + 1));
    End;
    if fListQuestion.Count <> fListAnswer.Count then
      raise Exception.Create('Неверно распарсена БД: fQ.Count<>fA.Count');
  finally
    aFile.Free;
  end;
end;

function TVictorine.RandomQuitz: String;
begin
  Randomize;
  if fListQuestion.Count > 0 then
    Result := fListQuestion[Random(fListQuestion.Count - 1)];
end;

procedure TVictorine.SaveToFile(const FileName: String);
var
  aFile: THashedStringList;
  I: Integer;
begin
  aFile := THashedStringList.Create;
  try
    for I := 0 to fListQuestion.Count - 1 do
      aFile.Add(fListQuestion[I] + '|' + fListAnswer[I]);
    aFile.SaveToFile(FileName);
  finally
    aFile.Free;
  end;
end;

class function TVictorine.Split(const Text: String; Delim: String): TArray<String>;
var
  SL: THashedStringList;
  x: String;
  I: Integer;
begin
  SL := THashedStringList.Create;
  x := Text;
  try
    while Length(x) > 0 do
    Begin
      if Pos(Delim, x) > 0 then
      Begin
        SL.Add(Copy(x, 1, Pos(Delim, x) - 1));
        Delete(x, 1, Pos(Delim, x));
      End
      else
      Begin
        SL.Add(x);
        x := '';
      End;
    End;
    SetLength(Result, SL.Count);
    for I := 0 to SL.Count - 1 do
      Result[I] := SL[I];
  finally
    SL.Free;
  end;
end;

class function TVictorine.StopTrap(const Text: String): String;
begin
  Result := WideLowerCase(Text);
  Result := StringReplace(Result, 'a', 'а', [rfReplaceAll]);
  Result := StringReplace(Result, 'e', 'е', [rfReplaceAll]);
  Result := StringReplace(Result, 'p', 'р', [rfReplaceAll]);
  Result := StringReplace(Result, #99, #1089, [rfReplaceAll]);
  Result := StringReplace(Result, 'x', 'х', [rfReplaceAll]);
  Result := StringReplace(Result, 'y', 'у', [rfReplaceAll]);
  Result := StringReplace(Result, 'o', 'о', [rfReplaceAll]);
  Result := StringReplace(Result, 'k', 'к', [rfReplaceAll]);

  Result := StringReplace(Result, 't', 'т', [rfReplaceAll]);
  Result := StringReplace(Result, 'h', 'н', [rfReplaceAll]);
  Result := StringReplace(Result, 'c', 'с', [rfReplaceAll]);
  Result := StringReplace(Result, 'b', 'в', [rfReplaceAll]);
  Result := StringReplace(Result, 'm', 'м', [rfReplaceAll]);
  Result := StringReplace(Result, '?', '', [rfReplaceAll]);
end;

end.
