$PBExportHeader$w_mant_deta_valofactprod.srw
$PBExportComments$Mantención Valores de Facturación por Productor.
forward
global type w_mant_deta_valofactprod from w_mant_detalle
end type
end forward

global type w_mant_deta_valofactprod from w_mant_detalle
integer width = 2711
integer height = 1608
end type
global w_mant_deta_valofactprod w_mant_deta_valofactprod

type variables
DataWindowChild	idwc_especie, idwc_variedad, idwc_calibre, idwc_cliente, idwc_TipoVida
Date id_fechaini, id_fechafin

uo_Calibre				iuo_Calibre
uo_TipoVida				iuo_Tipo
uo_EmbalajesProd		iuo_Embalajes
uo_SemanaFactura	iuo_Semana
end variables

forward prototypes
public function boolean duplicado (string as_columna, string as_valor)
public function boolean noexistecalibre (string as_calibre)
public function boolean buscarango_especie (string as_especie)
public function boolean rangotemporada ()
end prototypes

public function boolean duplicado (string as_columna, string as_valor);Long		ll_fila
String		ls_especie, ls_variedad, ls_calibre, ls_embalaje, ls_TipoVida
Date		ld_fecini, ld_fecter

ls_especie	=	String(dw_1.GetItemNumber(il_fila,"espe_codigo"))
ls_variedad	=	String(dw_1.GetItemNumber(il_fila,"vari_codigo"))
ls_calibre	=	dw_1.GetItemString(il_fila,"vaca_calibr")
ld_fecini		=	dw_1.Object.vafa_fecini[il_fila]
ld_fecter		=	dw_1.Object.vafa_fecter[il_fila]
ls_embalaje =	dw_1.Object.emba_codigo[il_fila]
ls_TipoVida	=	String(dw_1.Object.emba_tipvid[il_fila])

CHOOSE CASE as_columna
	CASE "espe_codigo"
		ls_especie	= as_valor
		
	CASE "vari_codigo"
		ls_variedad	= as_valor
				
	CASE "vaca_calibr"
		ls_calibre	= as_valor
	
	CASE "vafa_fecini"
		ld_fecini	= Date(as_valor)
				
	CASE "vafa_fecter"
		ld_fecter	= Date(as_valor)
		
	CASE "emba_codigo"
		ls_embalaje	= as_valor
	
	Case 'emba_tipvid'
		ls_TipoVida = as_Valor
		
END CHOOSE

ll_fila	= istr_mant.dw.Find("espe_codigo = " + ls_especie + " and " + "vari_codigo = " + ls_variedad + " and " + &
							"String(vafa_fecini,'yyyy-mm-dd') = '" + String(ld_fecini,'yyyy-mm-dd') + "' and " + &
							"emba_codigo = '" + ls_embalaje + "'" +" and " + "emba_tipvid = " + ls_TipoVida + " and " + &
							"String(vafa_fecter,'yyyy-mm-dd') = '" + String(ld_fecter,'yyyy-mm-dd') + "' and " + &
							"vaca_calibr = '" + ls_calibre + "'", 1, istr_mant.dw.RowCount())

IF ll_fila > 0 and ll_fila <> il_filaAnc THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

public function boolean noexistecalibre (string as_calibre);Integer li_Contador, li_Especie, li_Variedad

li_Especie	=	dw_1.Object.espe_codigo[il_Fila]
li_Variedad	=	dw_1.Object.vari_codigo[il_Fila]

SELECT	Count(*)
INTO		:li_Contador
FROM		dbo.variecalibre
WHERE		vari_codigo = 	:li_Variedad
AND      espe_codigo = 	:li_Especie
AND      vaca_calibr	=	:as_calibre;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla VarieCalibre")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	Messagebox("Atención","Calibre no Existe.~r~r" + &
					"Ingrese o seleccione otro.",StopSign!)
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF
end function

public function boolean buscarango_especie (string as_especie);Long		ll_fila
String	ls_especie, ls_variedad, ls_calibre, ls_embalaje
Date		ld_fecini, ld_fecter

ls_especie	= as_especie

ll_fila	= dw_1.Find("espe_codigo = " + ls_especie ,1, istr_mant.dw.RowCount())

IF ll_fila > 0 and ll_fila <> il_filaAnc THEN
	
	dw_1.SetItem(il_fila, "vafa_fecini", dw_1.Object.vafa_fecini[ll_fila])
	dw_1.SetItem(il_fila, "vafa_fecter", dw_1.Object.vafa_fecter[ll_fila])
	
	RETURN True
ELSE	
	RETURN False
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

on w_mant_deta_valofactprod.create
call super::create
end on

on w_mant_deta_valofactprod.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[1]  = String(dw_1.GetItemNumber(il_fila, "espe_codigo"))
ias_campo[2]  = String(dw_1.GetItemNumber(il_fila, "vari_codigo"))
ias_campo[3]  = dw_1.GetItemString(il_fila, "vaca_calibr")
ias_campo[4]  = String(dw_1.GetItemDecimal(il_fila, "vafp_preuni"))
ias_campo[5]  = dw_1.GetItemString(il_fila, "emba_codigo")

IF ias_campo[3] = '-1' THEN
	dw_1.SetItem(il_fila, "todoscal", 1)
ELSE
	dw_1.SetItem(il_fila, "todoscal", 0)
END IF

IF ias_campo[5] = '-1' THEN
	dw_1.SetItem(il_fila, "todos", 1)
ELSE
	dw_1.SetItem(il_fila, "todos", 0)
END IF

dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[1]))
dw_1.SetItem(il_fila, "prod_codigo", Long(istr_mant.argumento[2]))
dw_1.SetItem(il_fila, "prod_nombre", istr_mant.argumento[3])

dw_1.SetItemStatus(il_fila, "clie_codigo", Primary!, NotModified!)
dw_1.SetItemStatus(il_fila, "prod_codigo", Primary!, NotModified!)
dw_1.SetItemStatus(il_fila, "prod_nombre", Primary!, NotModified!)

IF istr_mant.agrega = False THEN
		dw_1.GetChild("vari_codigo", idwc_variedad)
		idwc_variedad.SetTransObject(sqlca)
		idwc_variedad.Retrieve(Integer(ias_campo[1]))
		dw_1.GetChild("vaca_calibr", idwc_calibre)
		idwc_calibre.SetTransObject(sqlca)
		idwc_calibre.Retrieve(Integer(ias_campo[1]), Integer(ias_campo[2]))	
ELSE
	dw_1.Object.vafa_fecini[il_fila] = id_fechaini 
	dw_1.Object.vafa_fecter[il_fila] = id_fechafin		
END IF
end event

event ue_nuevo;call super::ue_nuevo;dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[1]))
dw_1.SetItem(il_fila, "prod_codigo", Long(istr_mant.argumento[2]))
dw_1.SetItem(il_fila, "prod_nombre", istr_mant.argumento[3])

dw_1.SetItemStatus(il_fila, "clie_codigo", Primary!, NotModified!)
dw_1.SetItemStatus(il_fila, "prod_codigo", Primary!, NotModified!)
dw_1.SetItemStatus(il_fila, "prod_nombre", Primary!, NotModified!)

IF dw_1.RowCount() > 1 THEN
	dw_1.SetItem(il_fila, "espe_codigo", dw_1.Object.espe_codigo[il_fila - 1])
	dw_1.SetItem(il_fila, "vari_codigo", dw_1.Object.vari_codigo[il_fila - 1])
	dw_1.SetItem(il_fila, "vafa_fecini", dw_1.Object.vafa_fecini[il_fila - 1])
	dw_1.SetItem(il_fila, "vafa_fecter", dw_1.Object.vafa_fecter[il_fila - 1])
//	dw_1.SetItem(il_fila, "emba_codigo", dw_1.Object.emba_codigo[il_fila - 1])
//	dw_1.SetItem(il_fila, "vaca_calibr", dw_1.Object.vaca_calibr[il_fila - 1])
//	
//	IF dw_1.Object.emba_codigo[il_fila - 1] <> '-1' THEN
//		dw_1.Object.todos[il_fila] = 0
//	END IF
//	
//	IF dw_1.Object.vaca_calibr[il_fila - 1] <> '-1' THEN
//		dw_1.Object.todoscal[il_fila] = 0
//	END IF
	
	dw_1.SetColumn('espe_codigo')
END IF	

end event

event ue_deshace;call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.SetItem(il_fila, "espe_codigo", Integer(ias_campo[1]))
	dw_1.SetItem(il_fila, "vari_codigo", Integer(ias_campo[2]))
	dw_1.SetItem(il_fila, "vaca_calibr", ias_campo[3])
	dw_1.SetItem(il_fila, "vafp_preuni", Dec(ias_campo[4]))
	dw_1.SetItem(il_fila, "emba_codigo", ias_campo[5])
	
	IF ias_campo[3] = '-1' THEN
		dw_1.SetItem(il_fila, "todoscal", 1)
	ELSE
		dw_1.SetItem(il_fila, "todoscal", 0)
	END IF
	
	IF ias_campo[5] = '-1' THEN
		dw_1.SetItem(il_fila, "todos", 1)
	ELSE
		dw_1.SetItem(il_fila, "todos", 0)
	END IF
END IF
end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]


IF Isnull(dw_1.GetItemNumber(il_fila, "espe_codigo")) OR dw_1.GetItemNumber(il_fila, "espe_codigo") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nEspecie "
	ls_colu[li_cont]	= "espe_codigo"
END IF

IF Isnull(dw_1.GetItemNumber(il_fila, "vari_codigo")) OR dw_1.GetItemNumber(il_fila, "vari_codigo") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nVariedad "
	ls_colu[li_cont]	= "vari_codigo"
END IF

IF Isnull(dw_1.GetItemString(il_fila, "vaca_calibr")) OR dw_1.GetItemString(il_fila, "vaca_calibr") = "" THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCalibre "
	ls_colu[li_cont]	= "vaca_calibr"
END IF

IF Isnull(dw_1.GetItemDecimal(il_fila, "vafp_preuni")) OR dw_1.GetItemDecimal(il_fila, "vafp_preuni") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nPrecio Unitario "
	ls_colu[li_cont]	= "vafp_preuni"
END IF

IF Isnull(dw_1.Object.vafa_fecini[il_fila]) OR dw_1.Object.vafa_fecini[il_fila] = date("0000/00/00") THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nFecha Inicial"
	ls_colu[li_cont]	= "vafa_fecini"
END IF

IF Isnull(dw_1.Object.vafa_fecter[il_fila]) OR dw_1.Object.vafa_fecter[il_fila] = date("0000/00/00") THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nFecha Término"
	ls_colu[li_cont]	= "vafa_fecter"
END IF

IF Isnull(dw_1.Object.emba_codigo[il_fila]) OR dw_1.Object.emba_codigo[il_fila] = "" THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo Embalaje"
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
						[2]	=	Código de Productor
						[3]	=	Nombre de Productor
						[4]	=	Código de Zona
*/
x	= 100
y	= 450

This.Icon	=	Gstr_apl.Icono

PostEvent("ue_recuperadatos")

rangotemporada()

iuo_calibre   	=	Create uo_calibre
iuo_Tipo			=	Create uo_TipoVida
iuo_Embalajes	=	Create uo_EmbalajesProd
iuo_Semana		=	Create uo_SemanaFactura

istr_mant = Message.PowerObjectParm

dw_1.GetChild("clie_codigo", idwc_cliente)
dw_1.GetChild("espe_codigo", idwc_especie)
dw_1.GetChild("vari_codigo", idwc_variedad)
dw_1.GetChild("vaca_calibr", idwc_calibre)
dw_1.GetChild("emba_tipvid", idwc_TipoVida)

idwc_cliente.SetTransObject(sqlca)
idwc_especie.SetTransObject(sqlca)
idwc_variedad.SetTransObject(sqlca)
idwc_calibre.SetTransObject(sqlca)
idwc_TipoVida.SetTransObject(sqlca)

idwc_cliente.Retrieve()
idwc_especie.Retrieve()
idwc_variedad.Retrieve(0)
idwc_TipoVida.Retrieve()
idwc_calibre.Retrieve(gi_CodEspecie, gi_CodVariedad)

//dw_1.SetTransObject(sqlca)
//istr_mant.dw.ShareData(dw_1)
end event

type pb_ultimo from w_mant_detalle`pb_ultimo within w_mant_deta_valofactprod
end type

type pb_siguiente from w_mant_detalle`pb_siguiente within w_mant_deta_valofactprod
end type

type pb_anterior from w_mant_detalle`pb_anterior within w_mant_deta_valofactprod
end type

type pb_primero from w_mant_detalle`pb_primero within w_mant_deta_valofactprod
end type

type pb_cancela from w_mant_detalle`pb_cancela within w_mant_deta_valofactprod
integer x = 2322
integer y = 544
end type

type pb_acepta from w_mant_detalle`pb_acepta within w_mant_deta_valofactprod
integer x = 2313
integer y = 276
end type

type pb_salir from w_mant_detalle`pb_salir within w_mant_deta_valofactprod
integer x = 2336
integer y = 800
end type

type dw_1 from w_mant_detalle`dw_1 within w_mant_deta_valofactprod
integer x = 37
integer y = 88
integer width = 2112
integer height = 1392
string dataobject = "dw_mant_valofactprod"
end type

event dw_1::itemchanged;call super::itemchanged;String		ls_Columna
Integer	li_Null
Date		ld_FecIni, ld_FecTer

SetNull(li_Null)

ls_Columna	=	dwo.Name

Choose Case ls_Columna
	Case 'semana'
		If Not iuo_Semana.of_Existe(Integer(Data), True, Sqlca) Then
			This.SetItem(row, ls_Columna, li_Null)
			Return 1
		Else
			This.SetItem(Row, "vafa_fecini", iuo_Semana.Desde)
			This.SetItem(Row, "vafa_fecter", iuo_Semana.Hasta)
		End If
		
	Case "espe_codigo"
		If Duplicado(ls_Columna,data) Then
			This.SetItem(row, ls_Columna, Integer(ias_campo[1]))
			Return 1
		End If
		
		BuscaRango_Especie(Data)
		
		idwc_variedad.Retrieve(Integer(data))
		idwc_calibre.Retrieve(Integer(data), 1)
		
	Case "vari_codigo"
		If Duplicado(ls_Columna,data) Then
			This.SetItem(row, ls_Columna, Integer(ias_campo[2]))
			Return 1
		End If
		
		If idwc_variedad.GetRow() > 0 Then
			This.Object.vari_nombre[row]	=	idwc_variedad.GetItemString(idwc_variedad.GetRow(),"vari_nombre")
		End If
		
		idwc_calibre.Retrieve(This.Object.espe_codigo[row], Integer(data))
		
	Case "emba_codigo"
		If Not iuo_Embalajes.Existe(This.Object.clie_codigo[Row], Data, True, Sqlca) OR Duplicado(ls_Columna,Data) Then
			This.SetItem(Row, ls_Columna, ias_campo[3])
			This.SetItem(Row, 'emba_tipvid', li_Null)
			Return 1
		Else
			This.SetItem(Row, 'emba_tipvid', iuo_Embalajes.TipoVida)
		End If	
	
	Case "todos"
		If data = '1' Then
			dw_1.Object.emba_codigo[row]	=	'-1'
			This.SetItem(Row, ls_Columna, li_Null)
			Return
		Else	
			dw_1.Object.emba_codigo[row]	=	''	
		End If

	Case 'emba_tipvid'
		If Not iuo_Tipo.Existe (Integer(Data), True, Sqlca) OR Duplicado(ls_Columna, Data) Then
			This.SetItem(Row, ls_Columna, li_Null)
			Return 1
		End If
		
	Case "vaca_calibr"		
		If NOT iuo_calibre.existe(This.Object.espe_codigo[row],This.Object.vari_codigo[Row], Data, True, SQLCA) Or &
								Duplicado(ls_Columna, Data) Then
			This.SetItem(row, ls_Columna, ias_campo[3])
			Return 1
		End If	

	Case "todoscal"
		If data = '1' Then
			dw_1.Object.vaca_calibr[row]	=	'-1'	
			Return
		Else	
			dw_1.Object.vaca_calibr[row]	=	''	
		End If				
		
	Case "vafa_fecini"	
		If	date(data) < id_fechaini Then
			MessageBox("Error","Fecha NO puede ser Menor a Fecha Inicio Temporada",Information!, Ok!)
			This.SetItem(row, ls_Columna, id_fechaini)
			Return 1
		End If	
		
		If	Date(data) > id_fechafin Then
			MessageBox("Error","Fecha NO puede ser Mayor a Fecha Término Temporada",Information!, Ok!)
			This.SetItem(row, ls_Columna, id_fechaini)
			Return 1
		End If			
		
		ld_Fecter	=	This.Object.vafa_fecter[row]
		ld_FecIni	=	Date(data)
		
		If Not IsNull(ld_Fecter) Then
			If ld_FecIni > ld_Fecter Then
				MessageBox("Error","Fecha NO puede ser Mayor a Fecha Término",Information!, Ok!)
				This.SetItem(row, ls_Columna, id_fechaini)
				Return 1
			End If
		End If
		
		If Duplicado(ls_Columna,data) Then
			This.SetItem(row, ls_Columna, Date(li_Null))
			Return 1
		End If	
		
	Case "vafa_fecter"
		If	Date(data) > id_fechafin Then
			MessageBox("Error","Fecha NO puede ser Mayor a Fecha Fin Temporada",Information!, Ok!)
			This.SetItem(row, ls_Columna, id_fechafin)
			Return 1
		End If
		
		If	Date(data) < id_fechaini Then
			MessageBox("Error","Fecha NO puede ser Menor a Fecha Inicio Temporada",Information!, Ok!)
			This.SetItem(row, ls_Columna, id_fechafin)
			Return 1
		End If
		
		ld_FecIni	=	This.Object.vafa_fecini[row]
		ld_FecTer	=	Date(data)
		
		If Not IsNull(ld_FecIni) Then
			If ld_FecIni > ld_Fecter Then
				MessageBox("Error","Fecha NO puede ser Menor a Fecha Inicio.",Information!, Ok!)
				This.SetItem(row, ls_Columna, id_fechafin)
				Return 1
			End If
		End If	
		
		If Duplicado(ls_Columna,data) Then
			This.SetItem(row, ls_Columna, Date(li_Null))
			Return 1
		End If
		
	Case "vafp_preuni"
		If Duplicado(ls_Columna,data) Then
			This.SetItem(row, ls_Columna, li_null)
			Return 1
		End If		
		
End Choose
end event

