unit InlineMode.Main;

interface

uses
  Telegapi.Bot,
  Telegapi.Classes,
  System.Generics.Collections,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    mmo1: TMemo;
    tlgrmbt1: TTelegramBot;
    mmo2: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure tlgrmbt1Updates(Sender: TTelegramBot; Updates: TArray<Telegapi.Classes.TtgUpdate>);
    procedure tlgrmbt1Error(Sender: TTelegramBot; const Code: Integer; const Message: string);
  private
    { Private declarations }
    procedure InlineWork(Sender: TTelegramBot; IQ: TtgInlineQuery);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
  tlgrmbt1.Token := {$I ..\token.inc};
  tlgrmbt1.IsReceiving := True;
end;

procedure TForm1.InlineWork(Sender: TTelegramBot; IQ: TtgInlineQuery);
var
  LAnswers: TObjectList<TtgInlineQueryResult>;
  LArticle: TtgInlineQueryResultArticle;
  I: Integer;
begin
  LAnswers := TObjectList<TtgInlineQueryResult>.Create;
  try
    for I := 0 to mmo1.Lines.Count - 1 do
      if mmo1.Lines.Names[I].Contains(IQ.Query) then
      Begin
        LArticle := TtgInlineQueryResultArticle.Create;

        LArticle.ID := Random(100000).ToString;
        LArticle.title := mmo1.Lines[I];
        LArticle.input_message_content := TtgInputTextMessageContent.Create;
        with TtgInputTextMessageContent(LArticle.input_message_content) do
        Begin
          message_text := mmo1.Lines.ValueFromIndex[I];
        End;
        LArticle.reply_markup := TtgInlineKeyboardMarkup.Create;
        LAnswers.Add(LArticle);
      End;
    if NOT tlgrmbt1.answerInlineQuery(IQ.ID, LAnswers.ToArray, 10) then
      Caption := 'Упс';
  finally
    LAnswers.Free;
  end;
end;

procedure TForm1.tlgrmbt1Error(Sender: TTelegramBot; const Code: Integer; const Message: string);
begin
  mmo2.Lines.Add(Code.ToString + ' ' + Message);
end;

procedure TForm1.tlgrmbt1Updates(Sender: TTelegramBot; Updates: TArray<Telegapi.Classes.TtgUpdate>);
var
  I: Integer;
  LMessage: TtgMessage;
begin
  for I := Low(Updates) to High(Updates) do
  Begin
    if assigned(Updates[I].Message) then
    Begin
      mmo2.Lines.Add(Updates[I].Message.text);
      if Updates[I].Message.text = 'Hi' then
      Begin
        LMessage := tlgrmbt1.sendMessage(Updates[I].Message.Chat.ID, '😍😘😊😁😂😬😀😜😠😡');
        FreeAndNil(LMessage);
      End;
    End;
    if assigned(Updates[I].InlineQuery) then
    Begin
      InlineWork(Sender, Updates[I].InlineQuery);
    End;
  End;
end;

end.
