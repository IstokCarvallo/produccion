$PBExportHeader$w_info_movimiento_envase_planta.srw
$PBExportComments$Informe de Movimiento de Envases Diarios por Plantas
forward
global type w_info_movimiento_envase_planta from w_para_informes
end type
type dw_1 from datawindow within w_info_movimiento_envase_planta
end type
type dw_2 from uo_dw within w_info_movimiento_envase_planta
end type
end forward

global type w_info_movimiento_envase_planta from w_para_informes
integer width = 2683
integer height = 1516
string title = "Movimiento de Envases Diarios por Plantas"
dw_1 dw_1
dw_2 dw_2
end type
global w_info_movimiento_envase_planta w_info_movimiento_envase_planta

type variables
DataWindowChild	idwc_planta, idwc_productor, idwc_tipoenvase, idwc_envase, idwc_exporta

uo_plantadesp			iuo_plantadesp
uo_productores			iuo_productores
uo_ClientesProd		iuo_Cliente

Integer	ii_Planta, ii_tipo_envase 




end variables

forward prototypes
public function boolean noexisteenvase (integer ai_envase, integer ai_tipo_enva)
public function boolean noexistetipoenvase (integer ai_tipo_enva)
end prototypes

public function boolean noexisteenvase (integer ai_envase, integer ai_tipo_enva);String ls_envase

SELECT	enva_nombre
	INTO	:ls_envase
	FROM	dba.envases
	WHERE	enva_codigo = 	:ai_envase AND
			enva_tipoen	=	:ai_tipo_enva;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla Envases")
	
	RETURN TRUE
ELSEIF sqlca.SQLCode = -1 THEN
	MessageBox("Atención", "Código de Envase (" + String(ai_envase) + &
					"), no ha sido creado en tabla respectiva.~r~r" + &
					"Ingrese o seleccione otro Código.")
	
	RETURN TRUE
END IF

RETURN FALSE





end function

public function boolean noexistetipoenvase (integer ai_tipo_enva);
String ls_tipo

SELECT	tien_nombre
	INTO	:ls_tipo
	FROM	dba.tiposenvases
	WHERE	enva_tipoen	=	:ai_tipo_enva ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla TiposEnvases")
	
	RETURN TRUE
ELSEIF sqlca.SQLCode = -1 THEN
	MessageBox("Atención", "Código de Tipo Envase (" + String(ai_tipo_enva) + &
					"), no ha sido creado en tabla respectiva.~r~r" + &
					"Ingrese o seleccione otro Código.")
	
	RETURN TRUE
END IF

RETURN FALSE


end function

on w_info_movimiento_envase_planta.create
int iCurrent
call super::create
this.dw_1=create dw_1
this.dw_2=create dw_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_1
this.Control[iCurrent+2]=this.dw_2
end on

on w_info_movimiento_envase_planta.destroy
call super::destroy
destroy(this.dw_1)
destroy(this.dw_2)
end on

event open;call super::open;x	=	0
y	=	0

iuo_plantadesp		=	Create uo_plantadesp
iuo_productores	=	Create uo_productores
iuo_Cliente			=	Create uo_ClientesProd

////Exportador
dw_1.GetChild("exportador", idwc_exporta)
idwc_exporta.SetTransObject(sqlca)
If idwc_exporta.Retrieve() = 0 Then
	MessageBox("Atención","Falta Registrar Exportadores")
	idwc_exporta.InsertRow(0)
End If

////Planta
dw_1.GetChild("planta", idwc_planta)
idwc_planta.SetTransObject(sqlca)
If idwc_planta.Retrieve(gi_codexport) = 0 Then
	MessageBox("Atención","Falta Registrar Plantas")
	idwc_planta.InsertRow(0)
End If

//Productor
dw_1.GetChild("productor",idwc_productor)
idwc_productor.SetTransObject(Sqlca)
If idwc_productor.Retrieve(-1) = 0 Then
	MessageBox("Atención","Falta Registrar Productores")
	idwc_productor.InsertRow(0)
End If

//Tipos de Envase
dw_1.GetChild("tiposenvase",idwc_tipoenvase)
idwc_tipoenvase.SetTransObject(Sqlca)
If idwc_tipoenvase.Retrieve() = 0 Then
	MessageBox("Atención","Falta Registrar Tipos de Envase")
	idwc_tipoenvase.InsertRow(0)
End If

//Envases
dw_1.GetChild("envase",idwc_envase)
idwc_envase.SetTransObject(Sqlca)
If idwc_envase.Retrieve(-1) = 0 Then
	MessageBox("Atención","Falta Registrar Envases")
	idwc_envase.InsertRow(0)
End If

dw_1.SetTransObject(SQLCA)
dw_1.InsertRow(0)

Paramtemporada(gstr_paramtempo)
dw_1.SetItem(1,"fechad", gstr_paramtempo.fechainicio)
dw_1.SetItem(1,"fechah", Today())
dw_1.SetItem(1,"exportador",gi_codexport)
end event

event resize;call super::resize;dw_1.x = st_titulo.x
dw_1.y = 440
end event

type pb_excel from w_para_informes`pb_excel within w_info_movimiento_envase_planta
boolean visible = true
integer x = 2222
integer y = 244
boolean enabled = true
end type

event pb_excel::clicked;call super::clicked;SetPointer(HourGlass!)

Integer	li_Planta, li_tipoenvase, li_envase, li_Consfechad, li_exporta
Long		ll_Fila, ll_productor, ll_cierre
Date		ld_fechaHasta , ld_fechadesde, ld_fechaprueba
String 	ls_fecha, ls_path, ls_file
Boolean	lb_AutoCommit

dw_2.SetTransObject(sqlca)
 
dw_1.AcceptText()

//exportador
If dw_1.Object.todosplanta[1] = 1 Then
	li_exporta = -1
Else
	li_exporta = dw_1.Object.exportador[1]
	If IsNull(li_exporta) Then
		MessageBox( "Cliente Erróneo", "Falta seleccionar un Cliente.", StopSign!, Ok!)
		Return 1				 
	End If
End If

// Acepta Planta //
If dw_1.Object.todosplanta[1] = 1 Then
	li_Planta = -1
Else
	li_Planta = dw_1.Object.planta[1]
	If IsNull(li_Planta) Then
		MessageBox( "Planta Errónea", "Falta seleccionar una Planta.", StopSign!, Ok!)
		Return 1				 
   End If
End If

// Acepta Productor
If dw_1.Object.todosprod[1] = 1 Then
	ll_Productor = -1
	If dw_1.Object.consprod[1] = 1 Then	
		ll_Productor =	-9
	End If
Else
	ll_Productor = dw_1.Object.productor[1]
	If IsNull(ll_Productor) Then
		MessageBox( "Productor Erróneo", "Falta seleccionar un Productor.", StopSign!, Ok!)
		Return 1				 
   End If
End If

// Acepta Tipos de Envase //
If dw_1.Object.todostiposenva[1] = 1 Then
	 li_tipoenvase = -1
Else
	 li_tipoenvase = dw_1.Object.tiposenvase[1]
	If IsNull( li_tipoenvase) Then
		MessageBox( "Tipo de Envase Erróneo", "Falta seleccionar un Tipo de Envase.", StopSign!, Ok!)
		Return 1				 
   End If
End If

// Acepta Envase //
If dw_1.Object.todosenva[1] = 1 Then
	li_envase = -1
Else
	 li_envase = dw_1.Object.envase[1]
	If IsNull( li_envase) Then
		MessageBox( "Envase Erróneo", "Falta seleccionar un Envase.", StopSign!, Ok!)
		Return 1				 
   End If
End If

ld_fechadesde = dw_1.Object.fechad[1]

If IsNull(ld_fechadesde) or ld_fechadesde<= ld_fechaprueba Then
	MessageBox( "Fecha Erronea", "Se debe Ingresar una Fecha Desde.", StopSign!, Ok!)
	Return 1				 
End If

ls_fecha=mid(String(dw_1.Object.fechah[1],'dd/mm/yyyy'),1,10)
ld_fechahasta = Date(ls_fecha)

If IsNull(ld_fechahasta) or ld_fechahasta<=ld_fechaprueba Then
	MessageBox( "Fecha Erronea", "Se debe Ingresar una Fecha Hasta.", StopSign!, Ok!)
	Return 1				 
End If

If ld_fechadesde > ld_fechahasta Then
	MessageBox( "Fechas Erroneas", "La Fecha Hasta debe ser mayor o igual a la Fecha Desde.", StopSign!, Ok!)
	Return 1
End If	

If dw_1.Object.consfecha[1]= 1 Then	
	li_consfechad	 =		1
Else
	li_Consfechad	 =		0
End If

ll_fila = dw_2.Retrieve(li_Planta, ll_Productor, li_tipoenvase, li_envase, ld_Fechadesde, ld_fechaHasta, li_Consfechad, li_exporta)

IF ll_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE

	If GetFileSaveName( "Seleccione archivo",  ls_path, ls_file, "Excel", ".XLS Files (*.xls),*.xls" , "C:\") = -1 Then
		MessageBox('Error', 'No se encontro archivo solicitdo.' , StopSign!, OK! )
		Return -1
	End If

	If dw_2.SaveAs(ls_File, Excel8!, True) = -1 Then
		MessageBox('Error', 'No se pùdo generar archivo ('+ ls_file +') con informción solicitda.' , StopSign!, OK! )
		Return -1
	Else
		MessageBox('Atencion', 'Archivo ('+ ls_file +') generado satisfactoriamente.' , Information!, OK! )
	End If
END IF

SetPointer(Arrow!)

end event

type st_computador from w_para_informes`st_computador within w_info_movimiento_envase_planta
end type

type st_usuario from w_para_informes`st_usuario within w_info_movimiento_envase_planta
end type

type st_temporada from w_para_informes`st_temporada within w_info_movimiento_envase_planta
end type

type p_logo from w_para_informes`p_logo within w_info_movimiento_envase_planta
end type

type st_titulo from w_para_informes`st_titulo within w_info_movimiento_envase_planta
integer x = 110
integer width = 1984
string text = "Informe Movimiento de Envases Diarios por Plantas"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_movimiento_envase_planta
integer x = 2254
integer y = 536
integer taborder = 130
end type

event pb_acepta::clicked;SetPointer(Arrow!)
 
Integer	li_Planta, li_tipoenvase, li_envase, li_Consfechad, li_exporta
Long		ll_Fila, ll_productor
Date		ld_fechaHasta , ld_fechadesde, ld_fechaprueba
String 	ls_fecha
Boolean	lb_AutoCommit

istr_info.titulo	= 'MOVIMIENTO DE ENVASES DIARIOS POR PLANTAS'
OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_mov_envases_por_plantas_total"
dw_1.AcceptText()

//exportador
If dw_1.Object.todocliente[1] = 1 Then
	li_exporta = -1
Else
	li_exporta = dw_1.Object.exportador[1]
	If IsNull(li_exporta) Then
		MessageBox( "Cliente Erróneo", "Falta seleccionar un Cliente.", StopSign!, Ok!)
		Return 1				 
	End If
End If

// Acepta Planta //
If dw_1.Object.todosplanta[1] = 1 Then
	li_Planta = -1
Else
	li_Planta = dw_1.Object.planta[1]
	If IsNull(li_Planta) Then
		MessageBox( "Planta Errónea", "Falta seleccionar una Planta.", StopSign!, Ok!)
		Return 1				 
   End If
End If

// Acepta Productor
If dw_1.Object.todosprod[1] = 1 Then
	ll_Productor = -1
	If dw_1.Object.consprod[1] = 1 Then	
		ll_Productor =	-9
	End If
Else
	ll_Productor = dw_1.Object.productor[1]
	If IsNull(ll_Productor) Then
		MessageBox( "Productor Erróneo", "Falta seleccionar un Productor.", StopSign!, Ok!)
		Return 1				 
   End If
End If

// Acepta Tipos de Envase //
If dw_1.Object.todostiposenva[1] = 1 Then
	 li_tipoenvase = -1
Else
	 li_tipoenvase = dw_1.Object.tiposenvase[1]
	If IsNull( li_tipoenvase) Then
		MessageBox( "Tipo de Envase Erróneo", "Falta seleccionar un Tipo de Envase.", StopSign!, Ok!)
		Return 1				 
   End If
End If

// Acepta Envase //
If dw_1.Object.todosenva[1] = 1 Then
	li_envase = -1
Else
	 li_envase = dw_1.Object.envase[1]
	If IsNull( li_envase) Then
		MessageBox( "Envase Erróneo", "Falta seleccionar un Envase.", StopSign!, Ok!)
		Return 1				 
   End If
End If

ld_fechadesde = dw_1.Object.fechad[1]

If IsNull(ld_fechadesde) or ld_fechadesde<= ld_fechaprueba Then
	MessageBox( "Fecha Erronea", "Se debe Ingresar una Fecha Desde.", StopSign!, Ok!)
	Return 1				 
End If

ls_fecha=mid(String(dw_1.Object.fechah[1],'dd/mm/yyyy'),1,10)
ld_fechahasta = Date(ls_fecha)

If IsNull(ld_fechahasta) or ld_fechahasta<=ld_fechaprueba Then
	MessageBox( "Fecha Erronea", "Se debe Ingresar una Fecha Hasta.", StopSign!, Ok!)
	Return 1				 
End If

If ld_fechadesde > ld_fechahasta Then
	MessageBox( "Fechas Erroneas", "La Fecha Hasta debe ser mayor o igual a la Fecha Desde.", StopSign!, Ok!)
	Return 1
End If	

If dw_1.Object.consfecha[1]= 1 Then	
	li_consfechad	 =		1
Else
	li_Consfechad	 =		0
End If

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	TRUE
vinf.dw_1.SetTransObject(sqlca)

ll_Fila	=	vinf.dw_1.Retrieve(li_Planta, ll_Productor, li_tipoenvase, li_envase, ld_Fechadesde, ld_fechaHasta, li_Consfechad, li_exporta)
										 
sqlca.AutoCommit	=	lb_AutoCommit

If ll_Fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_Fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)

end event

type pb_salir from w_para_informes`pb_salir within w_info_movimiento_envase_planta
integer x = 2235
integer y = 772
integer taborder = 140
end type

type dw_1 from datawindow within w_info_movimiento_envase_planta
integer x = 110
integer y = 440
integer width = 1984
integer height = 684
integer taborder = 140
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_movto_envases_diarios_seleccion"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String		ls_Columna
Integer	li_Null
SetNull(li_Null)

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna
	Case 'exportador'
		If Not iuo_Cliente.Existe(Long(Data), True, Sqlca) Then
			This.SetItem(Row, ls_Columna, li_Null )
			Return 1
		End If
		
	CASE "todocliente"
		IF data = '1' THEN This.Object.exportador[row]		=	li_Null	
		
	CASE "planta"
		IF NOT iuo_PlantaDesp.Existe(Integer(data),True,Sqlca) THEN
			This.SetItem(1, "planta", li_Null )
			This.SetFocus()
			RETURN 1
		END IF				
	
	CASE "tiposenvase"
		IF noexistetipoenvase(Integer(data)) THEN
			This.SetItem(1, "tiposenvase", li_Null )
			This.Object.envase[row]	=	li_Null
      		dw_1.Object.todosenva.protect = 1
			This.SetFocus()
			RETURN 1
		ELSE
			This.Object.envase[row]	=	li_Null
	         dw_1.Object.todosenva.protect = 0
		   	IF idwc_envase.Retrieve(Integer(data)) = 0 THEN
			   MessageBox("Atención","Falta Registrar Envases asociados a Tipos de Envases")
			   RETURN 1
		   END IF
		END IF

	CASE "envase"
		IF noexisteenvase(this.Object.tiposenvase[row],Integer(data)) THEN
			This.SetItem(1, "envase", li_Null )
			This.SetFocus()
			RETURN 1
		END IF
		

	CASE "productor"
		IF NOT iuo_Productores.Existe(Long(data),True,SqlCa) THEN
			This.SetItem(1, "productor", li_Null)
			This.SetFocus()
			RETURN 1
		END IF

	CASE "todosplanta"
		IF data = '1' THEN
			This.Object.planta[row]		=	li_Null
		END IF

	CASE	"todostiposenva"
		IF	data = '1' THEN
			This.Object.tiposenvase[row]	=	li_Null
			dw_1.Object.envase[row]		= li_Null
			dw_1.Object.todosenva[row]	=	1
		END IF
		
	CASE	"todosprod"
		IF	data = '1' THEN This.Object.productor[row]	=	li_Null
	
	CASE	"todosenva"
		IF	data = '1' THEN This.Object.envase[row]	=	li_Null

END CHOOSE
end event

event itemerror;Return 1
end event

type dw_2 from uo_dw within w_info_movimiento_envase_planta
boolean visible = false
integer x = 1669
integer y = 40
integer width = 206
integer height = 156
integer taborder = 10
boolean bringtotop = true
boolean enabled = false
string dataobject = "dw_info_mov_envases_por_plantas"
boolean vscrollbar = false
end type

