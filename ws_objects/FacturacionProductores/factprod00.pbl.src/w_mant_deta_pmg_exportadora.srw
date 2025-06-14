﻿$PBExportHeader$w_mant_deta_pmg_exportadora.srw
forward
global type w_mant_deta_pmg_exportadora from w_mant_detalle
end type
type pb_calendario from picturebutton within w_mant_deta_pmg_exportadora
end type
type mc_calendario from monthcalendar within w_mant_deta_pmg_exportadora
end type
end forward

global type w_mant_deta_pmg_exportadora from w_mant_detalle
integer width = 2702
integer height = 1736
pb_calendario pb_calendario
mc_calendario mc_calendario
end type
global w_mant_deta_pmg_exportadora w_mant_deta_pmg_exportadora

type variables
DataWindowChild	idwc_especie, idwc_variedad, idwc_calibre, idwc_cliente, idwc_TipoVida, idwc_Color
Date id_fechaini, id_fechafin

uo_Calibre				iuo_Calibre
uo_TipoVida				iuo_Tipo
uo_EmbalajesProd		iuo_Embalajes
uo_SemanaFactura	iuo_Semana
uo_Color					iuo_Color
uo_Variedades			iuo_Variedad
end variables

forward prototypes
public function boolean wf_buscarango_especie (string as_especie)
public function boolean wf_rangotemporada ()
public function boolean wf_duplicado (string as_columna, string as_valor)
end prototypes

public function boolean wf_buscarango_especie (string as_especie);Long		ll_fila
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

public function boolean wf_rangotemporada ();
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

public function boolean wf_duplicado (string as_columna, string as_valor);Long		ll_fila
String		ls_especie, ls_variedad, ls_calibre, ls_embalaje, ls_TipoVida, ls_Color, ls_Busqueda
Date		ld_fecini, ld_fecter

ls_especie	=	String(dw_1.GetItemNumber(il_fila,"espe_codigo"))
ls_variedad	=	String(dw_1.GetItemNumber(il_fila,"vari_codigo"))
ls_calibre	=	dw_1.GetItemString(il_fila,"vaca_calibr")
ld_fecini		=	dw_1.Object.vafa_fecini[il_fila]
ld_fecter		=	dw_1.Object.vafa_fecter[il_fila]
ls_embalaje =	dw_1.Object.emba_codigo[il_fila]
ls_TipoVida	=	String(dw_1.Object.emba_tipvid[il_fila])
ls_Color		=	dw_1.Object.colo_nombre[il_fila]

Choose Case as_columna
	Case "espe_codigo"
		ls_especie	= as_valor
		
	Case "vari_codigo"
		ls_variedad	= as_valor
				
	Case "vaca_calibr"
		ls_calibre	= as_valor
	
	Case "vafa_fecini"
		ld_fecini	= Date(as_valor)
				
	Case "vafa_fecter"
		ld_fecter	= Date(as_valor)
		
	Case "emba_codigo"
		ls_embalaje	= as_valor
	
	Case 'emba_tipvid'
		ls_TipoVida = as_Valor
		
	Case 'colo_nombre'
		ls_Color = as_Valor
		
End Choose

ls_Busqueda = "espe_codigo = " + ls_especie + " and " + "vari_codigo = " + ls_variedad + " and " + &
							"String(vafa_fecini,'yyyy-mm-dd') = '" + String(ld_fecini,'yyyy-mm-dd') + "' and " + &
							"emba_codigo = '" + ls_embalaje + "'" +" and " + "emba_tipvid = " + ls_TipoVida + " and " + &
							"colo_nombre = '" + ls_Color + "'" +" and " + &
							"String(vafa_fecter,'yyyy-mm-dd') = '" + String(ld_fecter,'yyyy-mm-dd') + "' and " + &
							"vaca_calibr = '" + ls_calibre + "'"

ll_fila	= istr_mant.dw.Find(ls_Busqueda, 1, istr_mant.dw.RowCount())

If ll_fila > 0 and ll_fila <> il_filaAnc Then
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	Return True
Else
	Return False
End If
end function

on w_mant_deta_pmg_exportadora.create
int iCurrent
call super::create
this.pb_calendario=create pb_calendario
this.mc_calendario=create mc_calendario
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.pb_calendario
this.Control[iCurrent+2]=this.mc_calendario
end on

on w_mant_deta_pmg_exportadora.destroy
call super::destroy
destroy(this.pb_calendario)
destroy(this.mc_calendario)
end on

event ue_recuperadatos;call super::ue_recuperadatos;String	ls_Calibre 

ias_campo[1]  = String(dw_1.GetItemNumber(il_fila, "espe_codigo"))
ias_campo[2]  = String(dw_1.GetItemNumber(il_fila, "vari_codigo"))
ias_campo[3]  = dw_1.GetItemString(il_fila, "vaca_calibr")
ias_campo[4]  = String(dw_1.GetItemDecimal(il_fila, "vafp_preuni"))
ias_campo[5]  = dw_1.GetItemString(il_fila, "emba_codigo")
ias_campo[6]  = dw_1.GetItemString(il_fila, "colo_nombre")

If ias_campo[3] = '-1' Then
	dw_1.SetItem(il_fila, "todoscal", 1)
Else
	dw_1.SetItem(il_fila, "todoscal", 0)
End If

If ias_campo[5] = '-1' Then
	dw_1.SetItem(il_fila, "todos", 1)
	ls_Calibre = '*'
Else
	dw_1.SetItem(il_fila, "todos", 0)
End If

If ias_campo[6] = '-1' Then
	dw_1.SetItem(il_fila, "todosCol", 1)
Else
	dw_1.SetItem(il_fila, "todosCol", 0)
End If

dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[1]))
dw_1.SetItem(il_fila, "zona_codigo", Long(istr_mant.argumento[2]))
dw_1.SetItem(il_fila, "zona_nombre", istr_mant.argumento[3])
dw_1.SetItem(il_fila, "espe_codigo", Long(istr_mant.argumento[4]))

dw_1.SetItemStatus(il_fila, "clie_codigo", Primary!, NotModified!)
dw_1.SetItemStatus(il_fila, "zona_codigo", Primary!, NotModified!)
dw_1.SetItemStatus(il_fila, "zona_nombre", Primary!, NotModified!)
dw_1.SetItemStatus(il_fila, "espe_codigo", Primary!, NotModified!)

If Not istr_mant.Agrega Then
	dw_1.GetChild("vari_codigo", idwc_variedad)
	idwc_variedad.SetTransObject(sqlca)
	idwc_variedad.Retrieve(Integer(ias_campo[1]))
	
	dw_1.GetChild("vaca_calibr", idwc_calibre)
	idwc_calibre.SetTransObject(sqlca)
	idwc_calibre.Retrieve(Integer(ias_campo[1]), Integer(ias_campo[2]))
	
	dw_1.GetChild("colo_nombre", idwc_color)
	idwc_color.SetTransObject(sqlca)
	idwc_color.Retrieve(Integer(istr_mant.argumento[1]), Integer(ias_campo[1]), Integer(ias_campo[2]), ls_Calibre)	
Else
	dw_1.Object.vafa_fecini[il_fila] = id_fechaini 
	dw_1.Object.vafa_fecter[il_fila] = id_fechafin		
End If
end event

event ue_nuevo;call super::ue_nuevo;dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[1]))
dw_1.SetItem(il_fila, "zona_codigo", Long(istr_mant.argumento[2]))
dw_1.SetItem(il_fila, "zona_nombre", istr_mant.argumento[3])
dw_1.SetItem(il_fila, "espe_codigo", Integer(istr_mant.argumento[4]))

dw_1.SetItemStatus(il_fila, "clie_codigo", Primary!, NotModified!)
dw_1.SetItemStatus(il_fila, "zona_codigo", Primary!, NotModified!)
dw_1.SetItemStatus(il_fila, "zona_nombre", Primary!, NotModified!)
dw_1.SetItemStatus(il_fila, "espe_codigo", Primary!, NotModified!)

If dw_1.RowCount() > 1 Then
	dw_1.SetItem(il_fila, "vari_codigo", dw_1.Object.vari_codigo[il_fila - 1])
	dw_1.SetItem(il_fila, "vafa_fecini", dw_1.Object.vafa_fecini[il_fila - 1])
	dw_1.SetItem(il_fila, "vafa_fecter", dw_1.Object.vafa_fecter[il_fila - 1])	
	dw_1.SetColumn('vari_codigo')
End If	

end event

event ue_deshace;call super::ue_deshace;If UpperBound(ias_campo) > 0 Then
	dw_1.SetItem(il_fila, "espe_codigo", Integer(ias_campo[1]))
	dw_1.SetItem(il_fila, "vari_codigo", Integer(ias_campo[2]))
	dw_1.SetItem(il_fila, "vaca_calibr", ias_campo[3])
	dw_1.SetItem(il_fila, "vafp_preuni", Dec(ias_campo[4]))
	dw_1.SetItem(il_fila, "emba_codigo", ias_campo[5])
	dw_1.SetItem(il_fila, "colo_nombre", ias_campo[6])
	
	If ias_campo[3] = '-1' Then
		dw_1.SetItem(il_fila, "todoscal", 1)
	Else
		dw_1.SetItem(il_fila, "todoscal", 0)
	End If
	
	If ias_campo[5] = '-1' Then
		dw_1.SetItem(il_fila, "todos", 1)
	Else
		dw_1.SetItem(il_fila, "todos", 0)
	End If
	
	If ias_campo[6] = '-1' Then
		dw_1.SetItem(il_fila, "todosCol", 1)
	Else
		dw_1.SetItem(il_fila, "todosCol", 0)
	End If
	
End If
end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]

If IsNull(dw_1.GetItemNumber(il_fila, "vari_codigo")) OR dw_1.GetItemNumber(il_fila, "vari_codigo") = 0 Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nVariedad "
	ls_colu[li_cont]	= "vari_codigo"
End If

If IsNull(dw_1.GetItemString(il_fila, "vaca_calibr")) OR dw_1.GetItemString(il_fila, "vaca_calibr") = "" Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCalibre "
	ls_colu[li_cont]	= "vaca_calibr"
End If

If IsNull(dw_1.GetItemDecimal(il_fila, "vafp_preuni")) OR dw_1.GetItemDecimal(il_fila, "vafp_preuni") = 0 Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nPrecio Unitario "
	ls_colu[li_cont]	= "vafp_preuni"
End If

If IsNull(dw_1.Object.vafa_fecini[il_fila]) OR dw_1.Object.vafa_fecini[il_fila] = date("0000/00/00") Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nFecha Inicial"
	ls_colu[li_cont]	= "vafa_fecini"
End If

If IsNull(dw_1.Object.vafa_fecter[il_fila]) OR dw_1.Object.vafa_fecter[il_fila] = date("0000/00/00") Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nFecha Término"
	ls_colu[li_cont]	= "vafa_fecter"
End If

If IsNull(dw_1.Object.emba_codigo[il_fila]) OR dw_1.Object.emba_codigo[il_fila] = "" Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo Embalaje"
	ls_colu[li_cont]	= "emba_codigo"
End If

If IsNull(dw_1.GetItemNumber(il_fila, "emba_tipvid")) Or dw_1.GetItemNumber(il_fila, "emba_tipvid") = 0 Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nTipo de Vida"
	ls_colu[li_cont]	= "emba_tipvid"
End If

If li_cont > 0 Then
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
End If
end event

event open;x	= 100
y	= 450

This.Icon	=	Gstr_apl.Icono

PostEvent("ue_recuperadatos")

wf_RangoTemporada()

iuo_calibre   	=	Create uo_calibre
iuo_Tipo			=	Create uo_TipoVida
iuo_Embalajes	=	Create uo_EmbalajesProd
iuo_Semana		=	Create uo_SemanaFactura
iuo_Color		=	Create uo_Color
iuo_Variedad	=	Create uo_Variedades

istr_Mant = Message.PowerObjectParm

dw_1.GetChild("clie_codigo", idwc_cliente)
dw_1.GetChild("espe_codigo", idwc_especie)
dw_1.GetChild("vari_codigo", idwc_variedad)
dw_1.GetChild("vaca_calibr", idwc_calibre)
dw_1.GetChild("emba_tipvid", idwc_TipoVida)
dw_1.GetChild("colo_nombre", idwc_Color)

idwc_cliente.SetTransObject(sqlca)
idwc_especie.SetTransObject(sqlca)
idwc_variedad.SetTransObject(sqlca)
idwc_calibre.SetTransObject(sqlca)
idwc_TipoVida.SetTransObject(sqlca)
idwc_Color.SetTransObject(sqlca)

idwc_cliente.Retrieve()
idwc_especie.Retrieve()
idwc_variedad.Retrieve(0)
idwc_TipoVida.Retrieve()
idwc_Variedad.Retrieve(Integer(istr_Mant.Argumento[4]))
idwc_calibre.Retrieve(Integer(istr_Mant.Argumento[4]), gi_CodVariedad)
idwc_Color.Retrieve(gi_CodExport, Integer(istr_Mant.Argumento[4]), -1, '*')
		
end event

event resize;Integer		li_posic_x, li_posic_y, li_Ancho = 300, li_Alto = 245, li_Siguiente = 255

This.Height			=	dw_1.Height + 400
This.Width			=	dw_1.width + 600

dw_1.x				=	78
dw_1.y				=	100	

li_posic_x			=	This.WorkSpaceWidth() - 400
li_posic_y			=	dw_1.y

If pb_Acepta.Visible Then
	pb_Acepta.x			=	li_posic_x
	pb_Acepta.y			=	li_posic_y
	pb_Acepta.width	=	li_Ancho
	pb_Acepta.height	=	li_Alto
	li_posic_y+=li_Siguiente
End If

If pb_Cancela.Visible Then
	pb_Cancela.x		=	li_posic_x
	pb_Cancela.y		=	li_posic_y
	pb_Cancela.width	=	li_Ancho
	pb_Cancela.height	=	li_Alto
	li_posic_y+=li_Siguiente
End If

If pb_Calendario.Visible Then
	pb_Calendario.x		=	li_posic_x
	pb_Calendario.y		=	li_posic_y
	pb_Calendario.width	=	li_Ancho
	pb_Calendario.height	=	li_Alto
	li_posic_y+=li_Siguiente
End If

pb_salir.x			=	li_posic_x
pb_salir.y			=	li_posic_y
pb_salir.width		=	li_Ancho
pb_salir.height		=	li_Alto
end event

type pb_ultimo from w_mant_detalle`pb_ultimo within w_mant_deta_pmg_exportadora
end type

type pb_siguiente from w_mant_detalle`pb_siguiente within w_mant_deta_pmg_exportadora
end type

type pb_anterior from w_mant_detalle`pb_anterior within w_mant_deta_pmg_exportadora
end type

type pb_primero from w_mant_detalle`pb_primero within w_mant_deta_pmg_exportadora
end type

type pb_cancela from w_mant_detalle`pb_cancela within w_mant_deta_pmg_exportadora
integer x = 2322
integer y = 544
end type

type pb_acepta from w_mant_detalle`pb_acepta within w_mant_deta_pmg_exportadora
integer x = 2304
integer y = 272
end type

type pb_salir from w_mant_detalle`pb_salir within w_mant_deta_pmg_exportadora
integer x = 2336
integer y = 800
end type

type dw_1 from w_mant_detalle`dw_1 within w_mant_deta_pmg_exportadora
integer x = 37
integer y = 88
integer width = 2112
integer height = 1524
string dataobject = "dw_mant_pmg_exportadora"
end type

event dw_1::itemchanged;call super::itemchanged;String		ls_Columna
Integer	li_Null
Date		ld_FecIni, ld_FecTer

SetNull(li_Null)

ls_Columna	=	dwo.Name

Choose Case ls_Columna
	Case 'semana'
		If Not iuo_Semana.of_Existe(Integer(Data), True, Sqlca) Then
			This.SetItem(Row, ls_Columna, li_Null)
			Return 1
		Else
			This.SetItem(Row, "vafa_fecini", iuo_Semana.Desde)
			This.SetItem(Row, "vafa_fecter", iuo_Semana.Hasta)
		End If
		
	Case "vari_codigo"
		If wf_Duplicado(ls_Columna,data) Or Not iuo_Variedad.Existe(This.Object.espe_codigo[Row], Integer(Data), True, SQLCA) Then
			This.SetItem(Row, ls_Columna, Integer(ias_campo[2]))
			Return 1
		Else
			This.Object.vari_nombre[Row]	=	iuo_Variedad.NombreVariedad
			
			idwc_calibre.Retrieve(This.Object.espe_codigo[Row], Integer(data))
			idwc_Color.Retrieve(Integer(istr_Mant.Argumento[1]), This.Object.espe_codigo[Row], Integer(data), '*')
			If iuo_Color.of_Default(Integer(istr_Mant.Argumento[1]), This.Object.espe_codigo[Row], Integer(data), '*', False, SQLCA) Then
				This.SetItem(Row, "colo_nombre", iuo_Color.Color)
			End If
		End If
		
	Case "emba_codigo"
		If Not iuo_Embalajes.Existe(This.Object.clie_codigo[Row], Data, True, Sqlca) OR wf_Duplicado(ls_Columna,Data) Then
			This.SetItem(Row, ls_Columna, ias_campo[3])
			This.SetItem(Row, 'emba_tipvid', li_Null)
			Return 1
		Else
			This.SetItem(Row, 'emba_tipvid', iuo_Embalajes.TipoVida)
		End If	
	
	Case "todos"
		If data = '1' Then
			dw_1.Object.emba_codigo[Row]	=	'-1'
			This.SetItem(Row, ls_Columna, li_Null)
			Return
		Else	
			dw_1.Object.emba_codigo[Row]	=	''	
		End If

	Case 'emba_tipvid'
		If Not iuo_Tipo.Existe (Integer(Data), True, Sqlca) OR wf_Duplicado(ls_Columna, Data) Then
			This.SetItem(Row, ls_Columna, li_Null)
			Return 1
		End If
		
	Case "vaca_calibr"		
		If NOT iuo_calibre.existe(This.Object.espe_codigo[Row],This.Object.vari_codigo[Row], Data, True, SQLCA) Or &
								wf_Duplicado(ls_Columna, Data) Then
			This.SetItem(Row, ls_Columna, ias_campo[3])
			Return 1
		Else
			idwc_Color.Retrieve(Integer(istr_Mant.Argumento[1]), This.Object.espe_codigo[Row], This.Object.vari_codigo[Row], data)
			
			If iuo_Color.of_Default(Integer(istr_Mant.Argumento[1]), This.Object.espe_codigo[Row], This.Object.vari_codigo[Row], data, False, SQLCA) Then
				This.SetItem(Row, "colo_nombre", iuo_Color.Color)
			End If
		End If	

	Case "todoscal"
		If data = '1' Then
			dw_1.Object.vaca_calibr[Row]	=	'-1'	
			idwc_Color.Retrieve(Integer(istr_Mant.Argumento[1]), This.Object.espe_codigo[Row], This.Object.vari_codigo[Row], '*')
			
			If iuo_Color.of_Default(Integer(istr_Mant.Argumento[1]), This.Object.espe_codigo[Row], This.Object.vari_codigo[Row], '*', False, SQLCA) Then
				This.SetItem(Row, "colo_nombre", iuo_Color.Color)
			End If
			
			Return
		Else	
			dw_1.Object.vaca_calibr[Row]	=	''	
		End If				
		
	Case "vafa_fecini"	
		If	date(data) < id_fechaini Then
			MessageBox("Error","Fecha NO puede ser Menor a Fecha Inicio Temporada",Information!, Ok!)
			This.SetItem(Row, ls_Columna, id_fechaini)
			Return 1
		End If	
		
		If	Date(data) > id_fechafin Then
			MessageBox("Error","Fecha NO puede ser Mayor a Fecha Término Temporada",Information!, Ok!)
			This.SetItem(Row, ls_Columna, id_fechaini)
			Return 1
		End If			
		
		ld_Fecter	=	This.Object.vafa_fecter[Row]
		ld_FecIni	=	Date(data)
		
		If Not IsNull(ld_Fecter) Then
			If ld_FecIni > ld_Fecter Then
				MessageBox("Error","Fecha NO puede ser Mayor a Fecha Término",Information!, Ok!)
				This.SetItem(Row, ls_Columna, id_fechaini)
				Return 1
			End If
		End If
		
		If wf_Duplicado(ls_Columna,data) Then
			This.SetItem(Row, ls_Columna, Date(li_Null))
			Return 1
		End If	
		
	Case "vafa_fecter"
		If	Date(data) > id_fechafin Then
			MessageBox("Error","Fecha NO puede ser Mayor a Fecha Fin Temporada",Information!, Ok!)
			This.SetItem(Row, ls_Columna, id_fechafin)
			Return 1
		End If
		
		If	Date(data) < id_fechaini Then
			MessageBox("Error","Fecha NO puede ser Menor a Fecha Inicio Temporada",Information!, Ok!)
			This.SetItem(Row, ls_Columna, id_fechafin)
			Return 1
		End If
		
		ld_FecIni	=	This.Object.vafa_fecini[Row]
		ld_FecTer	=	Date(data)
		
		If Not IsNull(ld_FecIni) Then
			If ld_FecIni > ld_Fecter Then
				MessageBox("Error","Fecha NO puede ser Menor a Fecha Inicio.",Information!, Ok!)
				This.SetItem(Row, ls_Columna, id_fechafin)
				Return 1
			End If
		End If	
		
		If wf_Duplicado(ls_Columna,data) Then
			This.SetItem(Row, ls_Columna, Date(li_Null))
			Return 1
		End If
		
	Case "vafp_preuni"
		If wf_Duplicado(ls_Columna,data) Then
			This.SetItem(Row, ls_Columna, li_null)
			Return 1
		End If
		
	Case "colo_nombre"
		If wf_Duplicado(ls_Columna,data) Then
			This.SetItem(Row, ls_Columna, li_null)
			Return 1
		End If
	
	Case "todoscol"
		If data = '1' Then
			dw_1.Object.colo_nombre[Row]	=	'-1'	
			Return
		Else	
			dw_1.Object.colo_nombre[Row]	=	''
		End If	
		
End Choose
end event

type pb_calendario from picturebutton within w_mant_deta_pmg_exportadora
integer x = 2336
integer y = 1076
integer width = 302
integer height = 244
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Calendario.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Calendario-bn.png"
alignment htextalign = left!
end type

event clicked;If mc_Calendario.Visible Then
	mc_Calendario.Visible	= False
Else
	mc_Calendario.Visible	= True
End If
end event

type mc_calendario from monthcalendar within w_mant_deta_pmg_exportadora
boolean visible = false
integer x = 1115
integer y = 648
integer width = 1230
integer height = 664
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554431
long titletextcolor = 134217742
long trailingtextcolor = 134217745
long monthbackcolor = 1073741824
long titlebackcolor = 134217741
integer maxselectcount = 31
integer scrollrate = 1
boolean todaysection = true
boolean todaycircle = true
boolean weeknumbers = true
boolean border = true
borderstyle borderstyle = stylelowered!
boolean autosize = true
end type

event dateselected;Date 		StartDate, EndDate
Integer	li_Return

li_Return = mc_Calendario.GetSelectedDate(StartDate)

If li_Return = -1 Then  
	li_Return = mc_Calendario.GetSelectedRange(StartDate, EndDate)

	If li_Return = -1 Then   
		MessageBox("Seleccion Fecha",   "Porfavor seleccione una Fecha!")
	ElseIf li_return = 0 Then   
		dw_1.SetItem(1, "vafa_fecini", StartDate)
		dw_1.SetItem(1, "vafa_fecter", EndDate)
	Else   
		MessageBox("Seleccion Fecha",  "A ocurrido un error. Por favor vuelva a seleccionar una fecha!")
	End If
ElseIf li_Return = 0 Then
	dw_1.SetItem(1, "vafa_fecini", StartDate)
	dw_1.SetItem(1, "vafa_fecter", RelativeDate(StartDate, 7))
End If

This.Visible = False
end event

