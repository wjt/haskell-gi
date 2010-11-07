module GI.BaseInfo
 (
 )
where

import Foreign

#include <girepository.h>

{# pointer *GIBaseInfo as BaseInfo newtype #}
{# class BaseInfoClass BaseInfo #}

{# pointer *GICallableInfo as CallableInfo newtype #}
{# class BaseInfoClass => CallableInfoClass CallableInfo #}

{# pointer *GIFunctionInfo as FunctionInfo newtype #}
{# class CallableInfoClass => FunctionInfoClass FunctionInfo #}

{# pointer *GISignalInfo as SignalInfo newtype #}
{# class CallableInfoClass => SignalInfoClass SignalInfo #}

{# pointer *GIVFuncInfo as VFuncInfo newtype #}
{# class CallableInfoClass => VFuncInfoClass VFuncInfo #}

{# pointer *GIRegisteredTypeInfo as RegisteredTypeInfo newtype #}
{# class BaseInfoClass => RegisteredTypeInfoClass RegisteredTypeInfo #}

{# pointer *GIEnumInfo as EnumInfo newtype #}
{# class RegisteredTypeInfoClass => EnumInfoClass EnumInfo #}

{# pointer *GIInterfaceInfo as InterfaceInfo newtype #}
{# class RegisteredTypeInfoClass => InterfaceInfoClass InterfaceInfo #}

{# pointer *GIObjectInfo as ObjectInfo newtype #}
{# class RegisteredTypeInfoClass => ObjectInfoClass ObjectInfo #}

{# pointer *GIStructInfo as StructInfo newtype #}
{# class RegisteredTypeInfoClass => StructInfoClass StructInfo #}

{# pointer *GIUnionInfo as UnionInfo newtype #}
{# class RegisteredTypeInfoClass => UnionInfoClass UnionInfo #}

{# pointer *GIArgInfo as ArgInfo newtype #}
{# class BaseInfoClass => ArgInfoClass ArgInfo #}

{# pointer *GIConstantInfo as ConstantInfo newtype #}
{# class BaseInfoClass => ConstantInfoClass ConstantInfo #}

{# pointer *GIErrorDomainInfo as ErrorDomainInfo newtype #}
{# class BaseInfoClass => ErrorDomainInfoClass ErrorDomainInfo #}

{# pointer *GIFieldInfo as FieldInfo newtype #}
{# class BaseInfoClass => FieldInfoClass FieldInfo #}

{# pointer *GIPropertyInfo as PropertyInfo newtype #}
{# class BaseInfoClass => PropertyInfoClass PropertyInfo #}

{# pointer *GITypeInfo as TypeInfo newtype #}
{# class BaseInfoClass => TypeInfoClass TypeInfo #}

