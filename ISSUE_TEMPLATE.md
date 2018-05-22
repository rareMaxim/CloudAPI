### TelegaPi version/Версия TelegaPi
> v.3.5.5
### IDE version/ Версия IDE
> RAD Studio Berlin
### Information/Информация
> The SendVideo method does not work.
### Steps to reproduce the behavior/Что нужно сделать, для воспроизведения
> I call the method, similarly to SendFoto in the examples: tgBot.SendVideo (theChatID, TtgFileToSend.FromFile ('c:\testmp4\test.mp4'));
>I get: OnGlobalException: RequestAPI @ Could not convert variant of type (UnicodeString) into type (Double)
