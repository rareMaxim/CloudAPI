unit SendingPhotoMessageTests;

interface

uses
  DUnitX.TestFramework,
  TelegAPI.Bot,
  TelegAPI.Types;

type
  [TestFixture]
  TSendingPhotoMessageTests = class(TObject)
  strict private
    FBot: ITelegramBot;
    FTmpMsg: ITgMessage;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure Send_Photo_File;
    [Test]
    procedure Send_Photo_FileId;
    [Test]
    procedure Parse_Message_Caption_Entities_Into_Values;
    [Test]
    procedure Send_Photo_With_Markdown_Encoded_Caption;
  end;

implementation

uses
  System.SysUtils,
  TelegAPI.Bot.Impl,
  Test.Config,
  TelegAPI.Types.Enums,
  System.Generics.Collections;

procedure TSendingPhotoMessageTests.Parse_Message_Caption_Entities_Into_Values;
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
    LMessage := FBot.SendPhoto(//
      TestConfig.UserId, //
      TTestConst.Photos.Logo, string.Join(#13#10, LEntityData.Keys.ToArray));
    for LEnt in LMessage.CaptionEntities do
    begin
      Assert.IsTrue(LEntityData.ContainsValue(LEnt.TypeMessage));
    end;
  finally
    LEntityData.Free;
  end;

end;

procedure TSendingPhotoMessageTests.Send_Photo_File;
var
  LPhoto: ItgPhotoSize;
begin
  FTmpMsg := FBot.SendPhoto(TestConfig.UserId, TTestConst.Photos.Bot, 'This is a'#13#10 + 'Telegram Bot');
  Assert.AreEqual(TtgMessageType.Photo, FTmpMsg.&Type);
  Assert.IsNotEmpty(FTmpMsg.Photo);
  for LPhoto in FTmpMsg.Photo do
  begin
    Assert.IsNotEmpty(LPhoto.FileId);
    Assert.AreNotEqual(Default(Int64), LPhoto.Width);
    Assert.AreNotEqual(Default(Int64), LPhoto.Height);
  end;
  Assert.IsNotNull(FTmpMsg.From);
end;

procedure TSendingPhotoMessageTests.Send_Photo_FileId;
var
  LMessage: ITgMessage;
  I: Integer;
begin
  LMessage := FBot.SendPhoto(TestConfig.UserId, FTmpMsg.Photo[0].fileId);
  for I := 0 to Length(LMessage.Photo) - 1 do
  begin
    Assert.AreEqual(LMessage.Photo[I].FileId, FTmpMsg.Photo[I].FileId)
  end;
end;

procedure TSendingPhotoMessageTests.Send_Photo_With_Markdown_Encoded_Caption;
var
  LEntityData: TDictionary<TtgMessageEntityType, string>;
  LMessage: ITgMessage;
  LEnt: ItgMessageEntity;
begin
  LEntityData := TDictionary<TtgMessageEntityType, string>.Create();
  try
    LEntityData.Add(TtgMessageEntityType.Bold, '*bold*');
    LEntityData.Add(TtgMessageEntityType.italic, '_italic_');
    LEntityData.Add(TtgMessageEntityType.TextLink, '[Text Link](https://github.com/TelegramBots)');

    LMessage := FBot.SendPhoto(//
      TestConfig.UserId, //
      TTestConst.Photos.Logo,
      string.Join(#13#10, LEntityData.Values.ToArray), //
      TtgParseMode.Markdown);
    for LEnt in LMessage.CaptionEntities do
    begin
      Assert.IsTrue(LEntityData.ContainsKey(LEnt.TypeMessage));
    end;
  finally
    LEntityData.Free;
  end;
end;

procedure TSendingPhotoMessageTests.Setup;
begin
  FBot := TTelegramBot.Create(TestConfig.Token);
end;

procedure TSendingPhotoMessageTests.TearDown;
begin
  (FBot as TTelegramBot).Free;
  FBot := nil;
end;

initialization
  TDUnitX.RegisterTestFixture(TSendingPhotoMessageTests);

end.

