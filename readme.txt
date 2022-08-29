Созано в Lazarus 2.2.2 с использованием библиотеки Synapse 40.1 с некоторыми изменениями.
Для работы на разных дистрибутивах linux в файле ssl_openssl_lib.pas после строки 1882 добавлены следующие строки:
      {$IFDEF LINUX}
      if SSLUtilHandle = 0 then SSLUtilHandle := LoadLib('libcrypto.so.1.1');
      if SSLUtilHandle = 0 then SSLUtilHandle := LoadLib('libcrypto.so.3');
      if SSLLibHandle = 0 then SSLLibHandle := LoadLib('libssl.so.1.1');
      if SSLLibHandle = 0 then SSLLibHandle := LoadLib('libssl.so.3');
      {$ENDIF}
Включено в качестве ресурсов openssl-1.0.2u-i386-win32.zip с библиотеками SSL для Windows (взяты отсюда: https://github.com/IndySockets/OpenSSL-Binaries). 
Для Windows или собирайте в 32-разрядной версии Lazarus, или ставте целевой процессор 386 в параметрах проекта, чтоб работало под всеми Windows.
Ну или компилируйте 64 версию, но и библиотеки SSL тогда нужно положить для 64-разрядной версии, и поправить ресурсы. 
