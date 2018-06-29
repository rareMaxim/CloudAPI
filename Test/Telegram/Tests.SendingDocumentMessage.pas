unit Tests.SendingDocumentMessage;

interface

uses
  DUnitX.TestFramework,
  TelegAPI.Bot,
  Test.Config;

type
  [TestFixture]
  TSendingDocumentMessageTests = class(TObject)
  strict private
    FBot: ITelegramBot;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    // Sample Methods
    // Simple single Test
    [Test]
    procedure Send_Pdf_Document;
    [Test]
    procedure Send_Document_With_Farsi_Name;
  end;

implementation

uses
  TelegAPI.Bot.Impl,
  TelegAPI.Types,
  TelegAPI.Types.Enums;

procedure TSendingDocumentMessageTests.Setup;
begin
  FBot := TTelegramBot.Create(TestConfig.Token);
end;

procedure TSendingDocumentMessageTests.TearDown;
begin
  (FBot as TTelegramBot).Free;
  FBot := nil;
end;

procedure TSendingDocumentMessageTests.Send_Document_With_Farsi_Name;
const
  caption = 'تراژدی هملتشاهزاده دانمارک';
  mimeType = 'application/pdf';
  FileSize = 256984;
var
  LMessage: ITgMessage;
begin
  LMessage := FBot.SendDocument(TestConfig.UserId, TTestConst.Documents.Hamlet, caption);
  Assert.AreEqual(TtgMessageType.Document, LMessage.&Type);
  Assert.AreEqual(mimeType, LMessage.Document.MimeType);
  // Assert.InRange(Math.Abs(fileSize - LMessage.Document.FileSize), 0, 3500);
  // Assert.InRange(LMessage.Document.FileId.Length, 20, 40);
  Assert.AreEqual(caption, LMessage.Caption);
end;

procedure TSendingDocumentMessageTests.Send_Pdf_Document;
const
  caption = 'The Tragedy of Hamlet,'#13#10'Prince of Denmark';
  mimeType = 'application/pdf';
  FileSize = 256984;
var
  LMessage: ITgMessage;
begin
  LMessage := FBot.SendDocument(TestConfig.UserId, TTestConst.Documents.Hamlet, caption);
  Assert.AreEqual(TtgMessageType.Document, LMessage.&Type);
  Assert.AreEqual(mimeType, LMessage.Document.MimeType);
 // Assert.InRange(Math.Abs(fileSize - message.Document.FileSize), 0, 3500);
 // Assert.InRange(message.Document.FileId.Length, 20, 40);
  Assert.AreEqual(caption, LMessage.Caption);
end;

initialization
  TDUnitX.RegisterTestFixture(TSendingDocumentMessageTests);

end.

