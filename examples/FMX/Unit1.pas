unit Unit1;

interface

uses
  TelegAPI.Bot,
  TelegAPI.Classes,
  TelegAPI.Utils,
  dwsComp, dwsExprs, dwsStringResult, dwsFunctions, dwsSymbols, dwsMagicExprs, dwsExprList,
  System.Threading,
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls;

type

  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    FBot: TTelegramBot;
    FID: Int64;
    Procedure OnError(Sender: TObject; Const Code: Integer; Const Message: String);
    Procedure OnUpdates(Sender: TObject; Updates: TArray<TtgUpdate>);
    Procedure UpdateManager(AUpdate: TtgUpdate);
  public
    { Public declarations }
  end;

  TSendTg = class(TInternalMagicProcedure)
    procedure DoEvalProc(const args: TExprBaseListExec); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  FBot.IsReceiving := True;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FBot.IsReceiving := False;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
  FBot := TTelegramBot.Create(Self);
  FBot.OnError := OnError;
  FBot.OnUpdates := OnUpdates;

  FBot.Token := {$I ..\telegaToken.inc};
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FBot.Free;
end;

procedure TForm1.OnError(Sender: TObject; const Code: Integer; const Message: String);
begin
  Memo1.Lines.Add(string.Join(' ', ['Error', 'in', Sender.ClassName, 'Code =', Code, 'Message =',
    Message]));
end;

procedure TForm1.OnUpdates(Sender: TObject; Updates: TArray<TtgUpdate>);
begin
  TParallel.&For(low(Updates), High(Updates),
    Procedure(I: Integer)
    Begin
      UpdateManager(Updates[I]);
      Updates[I].Free;
    End);
end;

procedure TForm1.UpdateManager(AUpdate: TtgUpdate);
var
  LMessage: TtgMessage;
var
  dws: TDelphiWebScript;
  prog: IdwsProgram;
begin
  if NOT Assigned(AUpdate.Message) then
    Exit;
  Memo1.Lines.Add(AUpdate.Message.From.Username + ': ' + AUpdate.Message.Text);
  dws := TDelphiWebScript.Create(nil);
  try
    prog := dws.Compile(AUpdate.Message.Text);
    RegisterInternalProcedure(TSendTg, 'SendTg', ['msg', 'String']);
    FID := AUpdate.Message.From.ID;
    if prog.Msgs.Count = 0 then
    begin
      prog.Execute;
    end
    else
    begin
      LMessage := FBot.sendTextMessage(FID, prog.Msgs.AsInfo);
    end;
  finally
    LMessage.Free;
    dws.Free;
    // prog:=nil;
  end;
end;

{ TSendTg }

procedure TSendTg.DoEvalProc(const args: TExprBaseListExec);
var
  LMsg: TtgMessage;
begin
  LMsg := Form1.FBot.sendTextMessage(Form1.FID, args.AsString[0]);
  LMsg.Free;
end;

end.
