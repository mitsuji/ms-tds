# ms-tds: TDS Protocol implemented in Haskell

This library is a Haskell implementation of the Tabular Data Stream (TSD) protocol
used by Microsoft SQL Server and others.


## Related projects

* [mssql-simple](https://github.com/mitsuji/mssql-simple)
  : SQL Server client library implemented in Haskell  
  https://github.com/mitsuji/mssql-simple
  
* [mssql-simple-example](https://github.com/mitsuji/mssql-simple-example)
  : Usage example of mssql-simple  
  https://github.com/mitsuji/mssql-simple-example


## Advantage

* ODBC independent

* Implemented only with Haskell (Independent of other languages ​​and environments)

* Supports encryption at login

* Implemented as
  [binary](http://hackage.haskell.org/package/binary)
  package compatible data types

* Intended for use in both Client library and Server library implementations

* 7.1 Revision 1 (SQL Server 2000 SP1 and later)

* Tested with SQL Server 2008 R2, SQL Server 2014



## Todo

* Write tests
  * Write general tests
  * Test legacy data types


* Transaction support


* Implement data types
  * AltMetaData
  * AltRow
  * ColInfo


* Implement [Binary.put](http://hackage.haskell.org/package/binary-0.10.0.0/docs/Data-Binary.html#v:put)
  interface of
  * TokenStreams


* Implement [Binary.get](http://hackage.haskell.org/package/binary-0.10.0.0/docs/Data-Binary.html#v:get)
  interface of
  * Login7
  * SqlBatch
  * RpcRequest


* Implement more detailed types
  * Collation(DataStream)
  * GUID(Prelogin,DataStream)
  * NONCE(Prelogin)
  * TimeStamp(RCDlarge)
  * Login7: TDS Version
  * Login7: client program version
  * Login7: timezone
  * Login7: language
  * Login7: collation
  * RpcReqBatchProcId: ProcID
  * TSEnvChange: Type
  * TSLoginAck: Interface
  * TSReturnValue: Status


* Implement Flag interfaces
  * Header status
  * PLOEncription
  * PLOMars
  * Login7 flag1
  * Login7 flag2
  * Login7 sql type
  * Login7 flag3
  * RpcReqBatchProcId OptionFlags
  * RpcReqBatchPhrocName OptionFlags
  * RpcReqBatchParam StatusFlag
  * AltMetaDta Flags
  * MetaColumnData Flags
  * TSDone status
  * TSDoneInProc status
  * TSDoneProc status
  * TSReturnValue Flags


* ETC
  * Encrypt entire connection support
  * Mars support
  * SSPI support
  * FedAuth support
  * Variable PacketSize support
  * TDS protocol versions other than 7.1

  * Attention  
    https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-tds/dc28579f-49b1-4a78-9c5f-63fbda002d2e
  
  * Bulk Load  
    https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-tds/88176081-df75-4b24-bcfb-4c16ff03cbfa

  * Distributed Transaction  
    https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-tds/b4b78564-5440-4fc0-b5ef-c9e1925aaefe



