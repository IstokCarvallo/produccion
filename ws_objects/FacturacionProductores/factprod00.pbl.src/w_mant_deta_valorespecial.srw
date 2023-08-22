$PBExportHeader$w_mant_deta_valorespecial.srw
$PBExportComments$Mantención Valores de Facturación por Productor.
forward
global type w_mant_deta_valorespecial from w_mant_detalle
end type
end forward

global type w_mant_deta_valorespecial from w_mant_detalle
integer width = 2651
integer height = 1664
end type
global w_mant_deta_valorespecial w_mant_deta_valorespecial

type variables
DataWindowChild	idwc_especie, idwc_variedad, idwc_calibre, idwc_productores, idwc_cliente, idwc_zona

Integer		ii_especie
Date id_fechaini, id_fechafin

uo_calibre				iuo_calibre
uo_SemanaFactura	iuo_Semana
end variables

forward prototypes
public function boolean duplicado (string as_columna, string as_valor)
public function boolean noexisteembalaje (string as_embalaje)
public function boolean rangotemporada ()
end prototypes

public function boolean duplicado (string as_columna, string as_valor);Long		ll_fila
String	ls_especie, ls_variedad, ls_calibre, ls_productor, ls_embalaje
Date		ld_fecini, ld_fecter

ls_especie		=	String(dw_1.GetItemNumber(il_fila,"espe_codigo"))
ls_variedad		= 	String(dw_1.GetItemNumber(il_fila,"vari_codigo"))
ls_calibre		= 	dw_1.GetItemString(il_fila,"vaca_calibr")
ls_productor	= 	String(dw_1.GetItemNumber(il_fila,"prod_codigo"))
ld_fecini		= 	dw_1.Object.vaes_fecini[il_fila]
ld_fecter		= 	dw_1.Object.vaes_fecter[il_fila]
ls_embalaje 	= dw_1.Object.emba_codigo[il_fila]

CHOOSE CASE as_columna
	CASE "espe_codigo"
		ls_especie		= 	as_valor
		
	CASE "vari_codigo"
		ls_variedad 	= 	as_valor

	CASE "vaca_calibr"
		ls_calibre  	= 	as_valor

	CASE "prod_codigo"
		ls_productor	= 	as_valor

	CASE "vaes_fecini"
		ld_fecini		= 	Date(as_valor)
				
	CASE "vaes_fecter"
		ld_fecter		= 	Date(as_valor)
	
	CASE "emba_codigo"
		ls_embalaje	= String(as_valor)

END CHOOSE

							
ll_fila	= istr_mant.dw.Find("espe_codigo = " + ls_especie + " and " + &
                     "prod_codigo = " + ls_productor + " and " + &
							"String(vaes_fecini,'yyyy-mm-dd') = '" + String(ld_fecini,'yyyy-mm-dd') + "' and " + &
							"String(vaes_fecter,'yyyy-mm-dd') = '" + String(ld_fecter,'yyyy-mm-dd') + "' and " + &
							"emba_codigo = '" + ls_embalaje + "'" +" and " + &
                     "vari_codigo = " + ls_variedad + " and " + &
                     "vaca_calibr = '" + ls_calibre + "'", &	
                     1, istr_mant.dw.RowCount())


IF ll_fila > 0 and ll_fila <> il_filaAnc THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE	
	RETURN False
END IF
end function

public function boolean noexisteembalaje (string as_embalaje);Integer li_Cliente, li_contador

li_cliente	=	dw_1.Object.clie_codigo[il_Fila]

SELECT	Count(*)
INTO		:li_contador
FROM		dbo.embalajesprod
WHERE		clie_codigo = 	:li_cliente
AND      emba_codigo = 	:as_embalaje;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla embalajesprod")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	Messagebox("Atención","Embalaje no Existe.~r~r" + &
					"Ingrese o seleccione otro.",StopSign!)
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF
end function

public function boolean rangotemporada ();
SELECT	pate_inicio,pate_termin
INTO		:id_fechaini, :id_fechafin
FROM		dbo.paramtemporada
WHERE		pate_vigent = 1;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla paramtemporada")
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF
end function

on w_mant_deta_valorespecial.create
call super::create
end on

on w_mant_deta_valorespecial.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[1]  = String(dw_1.GetItemNumber(il_fila, "espe_codigo"))
ias_campo[2]  = String(dw_1.GetItemNumber(il_fila, "vari_codigo"))
ias_campo[3]  = dw_1.GetItemString(il_fila, "vaca_calibr")
ias_campo[4]  = String(dw_1.GetItemDecimal(il_fila, "vaes_preuni"))
ias_campo[5]  = String(dw_1.GetItemNumber(il_fila, "prod_codigo"))
ias_campo[6]  = dw_1.GetItemString(il_fila, "emba_codigo")

IF ias_campo[3] = '-1' THEN
	dw_1.SetItem(il_fila, "todoscal", 1)
ELSE
	dw_1.SetItem(il_fila, "todoscal", 0)
END IF

IF ias_campo[6] = '-1' THEN
	dw_1.SetItem(il_fila, "todos", 1)
ELSE
	dw_1.SetItem(il_fila, "todos", 0)
END IF

dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[1]))
dw_1.SetItem(il_fila, "zona_codigo", Integer(istr_mant.argumento[2]))

dw_1.SetItemStatus(il_fila, "clie_codigo", Primary!, NotModified!)

IF istr_mant.agrega = False THEN
	IF Integer(ias_campo[1]) = 11 OR Integer(ias_campo[1]) = 21 THEN
		dw_1.GetChild("vari_codigo", idwc_variedad)
		idwc_variedad.SetTransObject(sqlca)
		idwc_variedad.Retrieve(Integer(ias_campo[1]))
		dw_1.GetChild("vaca_calibr", idwc_calibre)
		idwc_calibre.SetTransObject(sqlca)
		idwc_calibre.Retrieve(Integer(ias_campo[1]), Integer(ias_campo[2]))	
		
      dw_1.SetItem(il_fila, "espe_codigo", Integer(ias_campo[1]))
      dw_1.SetItem(il_fila, "vari_codigo", Integer(ias_campo[2]))		
		dw_1.SetItem(il_fila, "vaca_calibr", ias_campo[3])		
	END IF
ELSE
	dw_1.Object.vaes_fecini[il_fila] = id_fechaini 
	dw_1.Object.vaes_fecter[il_fila] = id_fechafin		
END IF
end event

event ue_nuevo;call super::ue_nuevo;dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[1]))
dw_1.SetItem(il_fila, "zona_codigo", Integer(istr_mant.argumento[2]))

dw_1.SetItemStatus(il_fila, "clie_codigo", Primary!, NotModified!)

IF dw_1.RowCount() > 1 THEN
	dw_1.SetItem(il_fila, "espe_codigo", dw_1.Object.espe_codigo[il_fila - 1])
	dw_1.SetItem(il_fila, "vari_codigo", dw_1.Object.vari_codigo[il_fila - 1])
	dw_1.SetItem(il_fila, "vaes_fecini", dw_1.Object.vaes_fecini[il_fila - 1])
	dw_1.SetItem(il_fila, "vaes_fecter", dw_1.Object.vaes_fecter[il_fila - 1])
//	dw_1.SetItem(il_fila, "emba_codigo", dw_1.Object.emba_codigo[il_fila - 1])
	dw_1.SetItem(il_fila, "prod_codigo", dw_1.Object.prod_codigo[il_fila - 1])
//	dw_1.SetItem(il_fila, "vaca_calibr", dw_1.Object.vaca_calibr[il_fila - 1])	
	
//	IF dw_1.Object.emba_codigo[il_fila - 1] <> '-1' THEN
//		dw_1.Object.todos[il_fila] = 0
//	END IF
	
//	IF dw_1.Object.vaca_calibr[il_fila - 1] <> '-1' THEN
//		dw_1.Object.todoscal[il_fila] = 0
//	END IF
	
	dw_1.SetColumn('vaes_preuni')
END IF	
end event

event ue_deshace;call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.SetItem(il_fila, "espe_codigo", Integer(ias_campo[1]))
	dw_1.SetItem(il_fila, "vari_codigo", Integer(ias_campo[2]))
	dw_1.SetItem(il_fila, "vaca_calibr", ias_campo[3])
	dw_1.SetItem(il_fila, "vaes_preuni", Dec(ias_campo[4]))
	dw_1.SetItem(il_fila, "prod_codigo", Long(ias_campo[5]))	
	dw_1.SetItem(il_fila, "emba_codigo", ias_campo[6])
	
	IF ias_campo[3] = '-1' THEN
		dw_1.SetItem(il_fila, "todoscal", 1)
	ELSE
		dw_1.SetItem(il_fila, "todoscal", 0)
	END IF
	
	IF ias_campo[6] = '-1' THEN
		dw_1.SetItem(il_fila, "todos", 1)
	ELSE
		dw_1.SetItem(il_fila, "todos", 0)
	END IF
END IF
end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont, li_especie
String	ls_mensaje, ls_colu[]

li_especie	=	dw_1.GetItemNumber(il_fila, "espe_codigo")
IF Isnull(li_especie) OR li_especie = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nEspecie "
	ls_colu[li_cont]	= "espe_codigo"
END IF

IF dw_1.GetItemNumber(il_fila, "prod_codigo") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nProductor "
	ls_colu[li_cont]	= "prod_codigo"
END IF

//IF li_especie = 11 THEN
	IF dw_1.GetItemNumber(il_fila, "vari_codigo") = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nVariedad "
		ls_colu[li_cont]	= "vari_codigo"
	END IF
	
	IF dw_1.GetItemString(il_fila, "vaca_calibr") = "" THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCalibre "
		ls_colu[li_cont]	= "vaca_calibr"
	END IF
//END IF

IF Isnull(dw_1.GetItemDate(il_fila, "vaes_fecini")) THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nFecha Inicial "
	ls_colu[li_cont]	= "vaes_fecini"
END IF

IF Isnull(dw_1.GetItemDate(il_fila, "vaes_fecter")) THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nFecha Final "
	ls_colu[li_cont]	= "vaes_fecter"
END IF

IF Isnull(dw_1.GetItemDecimal(il_fila, "vaes_preuni")) OR dw_1.GetItemDecimal(il_fila, "vaes_preuni") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nPrecio Unitario "
	ls_colu[li_cont]	= "vaes_preuni"
END IF

IF Isnull(dw_1.GetItemString(il_fila, "emba_codigo")) OR dw_1.GetItemString(il_fila, "emba_codigo") = "" THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo Embalaje "
	ls_colu[li_cont]	= "emba_codigo"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event open;/*
	Argumentos :
						[1]	=	Código de Cliente
						[2]	=	Código de Zona
*/
x	= 100
y	= 450

This.Icon	=	Gstr_apl.Icono

PostEvent("ue_recuperadatos")

rangotemporada()

istr_mant = Message.PowerObjectParm

iuo_calibre   =	Create uo_calibre
iuo_Semana	=	Create uo_SemanaFactura

dw_1.GetChild("clie_codigo", idwc_cliente)
dw_1.GetChild("zona_codigo", idwc_zona)
dw_1.GetChild("espe_codigo", idwc_especie)
dw_1.GetChild("vari_codigo", idwc_variedad)
dw_1.GetChild("vaca_calibr", idwc_calibre)
dw_1.GetChild("prod_codigo", idwc_productores)

idwc_cliente.SetTransObject(sqlca)
idwc_zona.SetTransObject(sqlca)
idwc_especie.SetTransObject(sqlca)
idwc_variedad.SetTransObject(sqlca)
idwc_calibre.SetTransObject(sqlca)
idwc_productores.SetTransObject(sqlca)

idwc_cliente.Retrieve()
idwc_zona.Retrieve()
idwc_especie.Retrieve()
idwc_variedad.Retrieve(0)
idwc_calibre.Retrieve(gi_CodEspecie, gi_CodVariedad)
idwc_productores.Retrieve(Integer(istr_mant.Argumento[2]))

//dw_1.SetTransObject(sqlca)
//istr_mant.dw.ShareData(dw_1)
end event

type pb_ultimo from w_mant_detalle`pb_ultimo within w_mant_deta_valorespecial
end type

type pb_siguiente from w_mant_detalle`pb_siguiente within w_mant_deta_valorespecial
end type

type pb_anterior from w_mant_detalle`pb_anterior within w_mant_deta_valorespecial
end type

type pb_primero from w_mant_detalle`pb_primero within w_mant_deta_valorespecial
end type

type pb_cancela from w_mant_detalle`pb_cancela within w_mant_deta_valorespecial
integer x = 2359
integer y = 992
end type

type pb_acepta from w_mant_detalle`pb_acepta within w_mant_deta_valorespecial
integer x = 2359
integer y = 812
end type

type pb_salir from w_mant_detalle`pb_salir within w_mant_deta_valorespecial
integer x = 2359
integer y = 1172
end type

type dw_1 from w_mant_detalle`dw_1 within w_mant_deta_valorespecial
integer x = 82
integer y = 132
integer width = 2103
integer height = 1364
string dataobject = "dw_mant_valoespecial"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_columna, ls_null, ls_Calibre, ls_embalaje
Integer	li_especie, li_null, ll_filas, li_Variedad
Date		ld_FecIni, ld_FecTer, ld_Null

SetNull(ld_null)
SetNull(li_null)
SetNull(ls_null)

ls_columna	=	dwo.Name

CHOOSE CASE ls_columna
		Case 'semana'
		If Not iuo_Semana.of_Existe(Integer(Data), True, Sqlca) Then
			This.SetItem(row, ls_Columna, li_Null)
			Return 1
		Else
			This.SetItem(Row, "vaes_fecini", iuo_Semana.Desde)
			This.SetItem(Row, "vaes_fecter", iuo_Semana.Hasta)
		End If
		
	CASE "espe_codigo"
		If Duplicado(ls_columna,data) Then
			This.SetItem(row, ls_columna, Integer(ias_campo[1]))
			Return 1
		End If
		
		
		ii_especie = Integer(data)
		
		idwc_variedad.Retrieve(Integer(data))
		idwc_calibre.Retrieve(Integer(data),1)

	CASE "vari_codigo"
		If Duplicado(ls_columna,data) Then
			This.SetItem(row, ls_columna, Integer(ias_campo[2]))
			Return 1
		End If
		
		If idwc_variedad.GetRow() > 0 Then
			
   		This.Object.vari_nombre[row]	=	idwc_variedad.GetItemString(idwc_variedad.GetRow(),"vari_nombre")			
		End If
		
		idwc_calibre.SetTransObject(sqlca)
		idwc_calibre.Retrieve(ii_especie, Integer(data))
		
	CASE "emba_codigo"
		If dw_1.Object.todos[row] = 1 Then
			dw_1.Object.emba_codigo[row]	=	'-1'	
			
			Return
		End If	
		
		ls_embalaje	=	data
			
		If NoExisteEmbalaje(ls_embalaje) OR Duplicado(ls_columna,ls_embalaje) Then
			This.SetItem(row, ls_columna, ias_campo[3])
			Return 1
		Else
			dw_1.Object.emba_codigo[il_Fila]	=	ls_embalaje			
		End If	
	
	CASE "todos"
		If data = '1' Then
			dw_1.Object.emba_codigo[row]	=	'-1'	
			Return
		Else	
			dw_1.Object.emba_codigo[row]	=	''	
		End If			
		
   CASE "vaca_calibr"
		If dw_1.Object.todoscal[row] = 1 Then
			dw_1.Object.vaca_calibr[row]	=	'-1'	
			
			Return
		End If	
		
		ls_calibre	=	data

		li_Especie	=	dw_1.Object.espe_codigo[il_Fila]
		li_Variedad	=	dw_1.Object.vari_codigo[il_Fila]
		
		If NOT iuo_calibre.existe(li_especie,li_variedad,ls_calibre,True,sqlca) Then
			This.SetItem(row, ls_columna, ias_campo[3])
			Return 1
		Else	
			dw_1.Object.vaca_calibr[il_Fila]	=	ls_Calibre	
		End If	
		
	CASE "todoscal"
		If data = '1' Then
			dw_1.Object.vaca_calibr[row]	=	'-1'	
			Return
		Else	
			dw_1.Object.vaca_calibr[row]	=	''	
		End If		
		
	CASE "prod_codigo"
		If Duplicado(ls_columna,data) Then
			This.SetItem(row, ls_columna, Long(ias_campo[5]))
			Return 1
		End If
	
	
	CASE "vaes_fecini"	
		If	date(data) < id_fechaini Then
			MessageBox("Error","Fecha NO puede ser Menor a Fecha Inicio Temporada",Information!, Ok!)
			This.SetItem(row, ls_columna, id_fechaini)
			Return 1
		End If	
		
		If	date(data) > id_fechafin Then
			MessageBox("Error","Fecha NO puede ser Mayor a Fecha Término Temporada",Information!, Ok!)
			This.SetItem(row, ls_columna, id_fechaini)
			Return 1
		End If			
				
		ld_Fecter	=	This.Object.vaes_fecter[row]
		ld_FecIni	=	date(data)
		
		If Not IsNull(ld_Fecter) Then
			If ld_FecIni > ld_Fecter Then
				MessageBox("Error","Fecha NO puede ser Mayor a Fecha Término",Information!, Ok!)
				This.SetItem(row, ls_columna, id_fechaini)
				Return 1
			End If
		End If
		
		If Duplicado(ls_columna,data) Then
			This.SetItem(row, ls_columna, ld_Null)
			Return 1
		End If	
		
	CASE "vaes_fecter"	
		If	date(data) > id_fechafin Then
			MessageBox("Error","Fecha NO puede ser Mayor a Fecha Fin Temporada",Information!, Ok!)
			This.SetItem(row, ls_columna, id_fechafin)
			Return 1
		End If
		
		If	date(data) < id_fechaini Then
			MessageBox("Error","Fecha NO puede ser Menor a Fecha Inicio Temporada",Information!, Ok!)
			This.SetItem(row, ls_columna, id_fechafin)
			Return 1
		End If
		
		ld_FecIni	=	This.Object.vaes_fecini[row]
		ld_FecTer	=	date(data)
		
		If Not IsNull(ld_FecIni) Then
			If ld_FecIni > ld_Fecter Then
				MessageBox("Error","Fecha NO puede ser Menor a Fecha Inicio.",Information!, Ok!)
				This.SetItem(row, ls_columna, id_fechafin)
				Return 1
			End If
		End If			
		
		If Duplicado(ls_columna,data) Then
			This.SetItem(row, ls_columna, ld_Null)
			Return 1
		End If	
		
	CASE "vaes_preuni"
		If Duplicado(ls_columna,data) Then
			This.SetItem(row, ls_columna, li_null)
			Return 1
		End If			
		
End CHOOSE
end event

