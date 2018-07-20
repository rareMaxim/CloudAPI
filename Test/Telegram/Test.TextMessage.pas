unit Test.TextMessage;

interface

uses
  TelegAPi.Bot,
  TelegAPi.Types,
  DUnitX.TestFramework;

type
  //[TestFixture]
  TTestTextMessage = class(TObject)
  strict private
    FBot: TTelegramBot;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure SendTextMessage;
    [Test]
    procedure SendTextMessageToChanel;
    [Test]
    procedure ForwardMessage;
    [Test]
    procedure Parse_MarkDown_Entities;
    [Test]
    procedure Parse_Html_Entities;
    [Test]
    procedure Should_Parse_Message_Entities_Into_Values;
  end;

implementation

uses
  Test.Config,
  TelegAPi.Types.Enums,
  System.Generics.Collections,
  System.SysUtils;

procedure TTestTextMessage.SendTextMessageToChanel;
var
  text: string;
  LMessage: ITgMessage;
begin
  text := string.format('Hello members of channel %S', [TestConfig.Channel]);
  LMessage := FBot.SendMessage(TestConfig.Channel, text);
  Assert.AreEqual(text, LMessage.Text);
  Assert.AreEqual(TtgMessageType.Text, LMessage.&Type);
end;

procedure TTestTextMessage.Setup;
begin
  FBot := TTelegramBot.Create(TestConfig.Token);
end;

procedure TTestTextMessage.Should_Parse_Message_Entities_Into_Values;
var
  LEntityData: TDictionary<string, TtgMessageEntityType>;
  LMessage: ITgMessage;
  LEnt: ItgMessageEntity;
begin
  LEntityData := TDictionary<string, TtgMessageEntityType>.Create();
  try
    LEntityData.Add('#TelegramBots', TtgMessageEntityType.hashtag);
    LEntityData.Add('@BotFather', TtgMessageEntityType.Mention);
    LEntityData.Add('http://github.com/TelegramBots', TtgMessageEntityType.Url);
    LEntityData.Add('security@telegram.org', TtgMessageEntityType.email);
    LEntityData.Add('/test', TtgMessageEntityType.BotCommand);
    LEntityData.Add('/test@test_bot', TtgMessageEntityType.BotCommand);
    LMessage := FBot.SendMessage(//
      TestConfig.UserId, //
      string.Join(#13#10, LEntityData.Keys.ToArray));
    for LEnt in LMessage.Entities do
    begin
      Assert.IsTrue(LEntityData.ContainsValue(LEnt.TypeMessage));
    end;
  finally
    LEntityData.Free;
  end;
end;

procedure TTestTextMessage.TearDown;
begin
  (FBot as TTelegramBot).Free;
  FBot := nil;
end;

procedure TTestTextMessage.ForwardMessage;
const
  text: string = 'Hello world!';
var
  LMessage, LMessage2: ITgMessage;
begin
  LMessage := FBot.SendMessage(TestConfig.UserId, text);
  LMessage2 := FBot.ForwardMessage(TestConfig.UserId, LMessage.Chat.ID, LMessage.MessageId);

  Assert.AreEqual(TestConfig.UserId, LMessage2.Chat.ID);
  Assert.IsNull(LMessage2.ForwardFromChat);
  Assert.AreEqual(Default(Int64), LMessage2.ForwardFromMessageId);
  Assert.AreNotEqual(Default(TDateTime), LMessage2.ForwardDate);
end;

procedure TTestTextMessage.Parse_Html_Entities;
var
  LEntityData: TDictionary<TtgMessageEntityType, string>;
  LMessage: ITgMessage;
  LEnt: ItgMessageEntity;
begin
  LEntityData := TDictionary<TtgMessageEntityType, string>.Create();
  try
    LEntityData.Add(TtgMessageEntityType.Bold, '<b>bold</b>');
   // LEntityData.Add(TtgMessageEntityType.Bold, '<strong>strong</strong>');
    LEntityData.Add(TtgMessageEntityType.italic, '<i>italic</i>');
   // LEntityData.Add(TtgMessageEntityType.italic, '<em>&lt;em&gt;</em>');
    LEntityData.Add(TtgMessageEntityType.TextLink, '<a href="https://telegram.org/">inline url to Telegram.org</a>');
    LEntityData.Add(TtgMessageEntityType.TextMention, string.Format('<a href="tg://user?id=%d">UserMention</a>',
      [TestConfig.UserId]));
    LEntityData.Add(TtgMessageEntityType.code, 'inline <code>begin end.</code>');
    LEntityData.Add(TtgMessageEntityType.pre, '<pre>pre-formatted fixed-width code block</pre>');
    LMessage := FBot.SendMessage(//
      TestConfig.UserId, //
      string.Join(#13#10, LEntityData.Values.ToArray), //
      TtgParseMode.Html, //
      True);
    for LEnt in LMessage.Entities do
    begin
      Assert.IsTrue(LEntityData.ContainsKey(LEnt.TypeMessage));
    end;
//    for LVal in LEntityData.Values do
//    begin
//     Assert.contains(LMessage.Text, LVal);
//    end;
  finally
    LEntityData.Free;
  end;
end;

procedure TTestTextMessage.Parse_MarkDown_Entities;
var
  LEntityData: TDictionary<TtgMessageEntityType, string>;
  LMessage: ITgMessage;
  LEnt: ItgMessageEntity;
begin
  LEntityData := TDictionary<TtgMessageEntityType, string>.Create();
  try
    LEntityData.Add(TtgMessageEntityType.Bold, '*bold*');
    LEntityData.Add(TtgMessageEntityType.italic, '_italic_');
    LEntityData.Add(TtgMessageEntityType.TextLink, '[inline url to Telegram.org](https://telegram.org/)');
    LEntityData.Add(TtgMessageEntityType.TextMention, string.Format('[UserMention](tg://user?id=%d)',
      [TestConfig.UserId]));
    LEntityData.Add(TtgMessageEntityType.code, 'inline `begin end.`');
    LEntityData.Add(TtgMessageEntityType.pre, '```pre-formatted fixed-width code block```');
    LMessage := FBot.SendMessage(//
      TestConfig.UserId, //
      string.Join(#13#10, LEntityData.Values.ToArray), //
      TtgParseMode.Markdown, //
      True);
    for LEnt in LMessage.Entities do
    begin
      Assert.IsTrue(LEntityData.ContainsKey(LEnt.TypeMessage));
    end;
  finally
    LEntityData.Free;
  end;
end;

procedure TTestTextMessage.SendTextMessage;
const
  text: string = 'Hello world!';
var
  LMessage: ITgMessage;
begin
  LMessage := FBot.SendMessage(TestConfig.UserId, text);
  Assert.AreEqual(text, LMessage.text);
  Assert.AreEqual(TtgMessageType.Text, LMessage.&Type);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestTextMessage);

end.

