{******************************************************************************}
{                                                                              }
{  LazORM Project                                                              }
{                                                                              }
{  uloDatabaseConstants                                                        }
{                                                                              }
{  Description:                                                                }
{    This unit simply supplies constant values that are used by the project.   }
{    Constants should be here for the reason that they will not be subject to  }
{    change within the scope of the tool.                                      }
{                                                                              }
{  (EDIT!!!) TODO:                                                                       }
{    - Constants should be split between two files. One that is supplied along }
{      with the super classes of the objects in the generated code. In other   }
{      words the ones that are used in the product of the tool. Other          }
{      constants should be kept in a separate unit, and these would be the     }
{      constants that are used inside the tool.                                }
{                                                                              }
{  Copyright (c) 2013 Andreas Lorenzen                                         }
{                                                                              }
{******************************************************************************}

unit uloDatabaseConstants;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

const
  lo_SQL_NULL = 'NULL';
  lo_XML_NULL = '';
  lo_Str_NULL = '';

implementation

end.

