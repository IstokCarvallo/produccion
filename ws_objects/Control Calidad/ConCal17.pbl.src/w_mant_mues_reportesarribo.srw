$PBExportHeader$w_mant_mues_reportesarribo.srw
$PBExportComments$mantenedor de reportes de arribo
forward
global type w_mant_mues_reportesarribo from w_mant_directo
end type
type uo_seltipo from uo_seleccion_tipotransporte within w_mant_mues_reportesarribo
end type
type uo_selnaves from uo_seleccion_naves within w_mant_mues_reportesarribo
end type
type st_1 from statictext within w_mant_mues_reportesarribo
end type
type st_2 from statictext within w_mant_mues_reportesarribo
end type
type st_3 from statictext within w_mant_mues_reportesarribo
end type
type st_4 from statictext within w_mant_mues_reportesarribo
end type
type em_fecha from editmask within w_mant_mues_reportesarribo
end type
type em_semana from editmask within w_mant_mues_reportesarribo
end type
end forward

global type w_mant_mues_reportesarribo from w_mant_directo
integer width = 3410
integer height = 2096
string title = "Reportes de Arribo"
windowstate windowstate = maximized!
uo_seltipo uo_seltipo
uo_selnaves uo_selnaves
st_1 st_1
st_2 st_2
st_3 st_3
st_4 st_4
em_fecha em_fecha
em_semana em_semana
end type
global w_mant_mues_reportesarribo w_mant_mues_reportesarribo

type variables
DatawindowChild 		idwc_variedad
Integer					ii_Especie

uo_recibidores			iuo_Recibidor
uo_variedades			iuo_Variedad
uo_Productores			iuo_Productor
uo_DoctosArribo		iuo_Doctos
end variables

forward prototypes
public function boolean duplicado (string columna, string valor)
public function boolean wf_grabaimagenes ()
end prototypes

public function boolean duplicado (string columna, string valor);Long	ll_Fila, ll_Correlativo

ll_Correlativo	=	dw_1.Object.rear_numero[il_fila]

Choose Case columna
	Case "rear_numero"
		ll_Correlativo	=	Integer(valor)

End Choose

ll_fila	= dw_1.Find("rear_numero = " + String(ll_Correlativo), 1, dw_1.RowCount())

If ll_fila > 0 and ll_fila <> il_fila Then
	MessageBox("Error","Código de Etiqueta ya fue ingresado anteriormente",Information!, Ok!)
	Return True
Else
	Return False
End If

end function

public function boolean wf_grabaimagenes ();Boolean	lb_Retorno = True
Long		ll_Fila
String		ls_Archivo

If dw_1.RowCount() < 1 Then
	lb_Retorno = False
Else
	For ll_Fila = 1 To dw_1.RowCount()
		ls_Archivo	=	dw_1.Object.rear_rutas[ll_Fila] + dw_1.Object.rear_archiv[ll_Fila]
		
		iuo_Doctos.GrabaImagen(dw_1, ll_Fila, Sqlca, ls_Archivo)
	Next
End If

Return lb_Retorno
end function

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta

If IsNull(uo_SelTipo.Codigo) Or uo_SelTipo.Codigo = '*' Then
	MessageBox('Error de Consistencia', 'Se debe ingresar Tipo de Nave.')
	Return
End If

If IsNull(uo_SelNaves.Codigo) Or uo_SelNaves.Codigo = -1 Then
	MessageBox('Error de Consistencia', 'Se debe ingresar Código de Nave.')
	Return
End If

If IsNull(em_fecha.Text) Or em_fecha.Text = '' Then
	MessageBox('Error de Consistencia', 'Se debe ingresar Fecha de Zarpe.')
	Return
End If

uo_SelTipo.Bloquear(True)
uo_SelNaves.Bloquear(True)
em_Fecha.Enabled = False

Do
	ll_fila	= dw_1.Retrieve(gi_CodExport, ii_Especie, uo_SelTipo.Codigo, uo_SelNaves.Codigo, Date(em_Fecha.Text))

	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetFocus()
		il_fila					= 1
		pb_imprimir.Enabled	= True
		pb_insertar.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
	ELSE
		pb_insertar.Enabled	= True
		pb_insertar.SetFocus()
	END IF

LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event open;call super::open;Boolean	lb_Cerrar

If IsNull(uo_SelTipo.Codigo) 		Then lb_Cerrar	=	True
If IsNull(uo_SelNaves.Codigo) 	Then lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else	
	ii_Especie	= Integer(Message.StringParm)
	
	uo_SelTipo.Seleccion(False, False)	
	uo_SelNaves.Seleccion(False, False)
	
	uo_SelTipo.Codigo									=	'M'
	uo_SelTipo.dw_seleccion.Object.Codigo[1]	=	'M'
	uo_SelNaves.Filtra('M')
	
	dw_1.GetChild("vari_codigo", idwc_variedad)
	idwc_variedad.SetTransObject(sqlca)
	If idwc_variedad.Retrieve(ii_Especie) = 0 Then idwc_variedad.InsertRow(0)
	
	iuo_Recibidor	=	Create uo_recibidores
	iuo_Variedad	=	Create uo_variedades
	iuo_Productor	=	Create uo_Productores
	iuo_Doctos		=	Create uo_DoctosArribo
	
	buscar		= "Correlativo:Nrear_numero,Recibidor:Nreci_codigo,Variedad:Nvari_codigo,Productor:Nprod_codigo"
	ordenar		= "Correlativo:rear_numero,Recibidor:reci_codigo,Variedad:vari_codigo,Productor:prod_codigo"
	is_ultimacol	= "rear_ccajas"
	
End If
end event

on w_mant_mues_reportesarribo.create
int iCurrent
call super::create
this.uo_seltipo=create uo_seltipo
this.uo_selnaves=create uo_selnaves
this.st_1=create st_1
this.st_2=create st_2
this.st_3=create st_3
this.st_4=create st_4
this.em_fecha=create em_fecha
this.em_semana=create em_semana
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.uo_seltipo
this.Control[iCurrent+2]=this.uo_selnaves
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.st_3
this.Control[iCurrent+6]=this.st_4
this.Control[iCurrent+7]=this.em_fecha
this.Control[iCurrent+8]=this.em_semana
end on

on w_mant_mues_reportesarribo.destroy
call super::destroy
destroy(this.uo_seltipo)
destroy(this.uo_selnaves)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.st_4)
destroy(this.em_fecha)
destroy(this.em_semana)
end on

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "MANTENCIÓN REPORTES DE ARRIBO"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_reportearribo"
vinf.dw_1.SetTransObject(sqlca)

vinf.dw_1.GetChild("vari_codigo", idwc_variedad)
idwc_variedad.SetTransObject(sqlca)
If idwc_variedad.Retrieve(ii_Especie) = 0 Then idwc_variedad.InsertRow(0)

fila = vinf.dw_1.Retrieve(gi_CodExport, ii_Especie, uo_SelTipo.Codigo, uo_SelNaves.Codigo, Date(em_Fecha.Text))

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Sort()
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)

	END IF
END IF

SetPointer(Arrow!)
end event

event ue_antesguardar;call super::ue_antesguardar;Long		ll_Fila
Integer	li_cont
String		ls_mensaje, ls_colu[]

If dw_1.RowCount() > 0 Then
	For ll_Fila = 1 To dw_1.RowCount()
		If IsNull(dw_1.Object.rear_numero[ll_Fila]) OR dw_1.Object.rear_numero[ll_Fila] = 0 Then
			li_cont ++
			ls_mensaje 			= ls_mensaje + "~nNro. Reporte Arribo"
			ls_colu[li_cont]	= "rear_numero"
		End If
		
		If IsNull(dw_1.Object.reci_codigo[ll_Fila]) OR dw_1.Object.reci_codigo[ll_Fila] = 0 Then
			li_cont ++
			ls_mensaje 			= ls_mensaje + "~nCódigo Recibidor"
			ls_colu[li_cont]	= "reci_codigo"
		End If
		
		If IsNull(dw_1.Object.vari_codigo[ll_Fila]) OR dw_1.Object.vari_codigo[ll_Fila] = 0 Then
			li_cont ++
			ls_mensaje 			= ls_mensaje + "~nCódigo Variedad"
			ls_colu[li_cont]	= "vari_codigo"
		End If
		
		If IsNull(dw_1.Object.prod_codigo[ll_Fila]) OR dw_1.Object.prod_codigo[ll_Fila] = 0 Then
			li_cont ++
			ls_mensaje 			= ls_mensaje + "~nCódigo Productor"
			ls_colu[li_cont]	= "prod_codigo"
		End If
		
		dw_1.Object.clie_codigo[ll_Fila]	= gi_CodExport
		dw_1.Object.espe_codigo[ll_Fila]	= ii_Especie
		dw_1.Object.nave_tipotr[ll_Fila]	= uo_SelTipo.Codigo
		dw_1.Object.nave_codigo[ll_Fila]	= uo_SelNaves.Codigo
		dw_1.Object.rear_fechaz[ll_Fila]	= Date(em_Fecha.Text)
		dw_1.Object.rear_semliq[ll_Fila]	= Integer(em_Semana.Text)
	Next
	
	If li_cont > 0 Then
		MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
		dw_1.SetColumn(ls_colu[1])
		dw_1.SetFocus()
		Message.DoubleParm = -1
	End If
Else
	pb_Grabar.Enabled		=	False
	pb_Eliminar.Enabled	=	False
	pb_Imprimir.Enabled	=	False
End If
end event

event ue_nuevo;call super::ue_nuevo;dw_1.SetColumn('rear_numero')
end event

event ue_guardar;call super::ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

If Message.DoubleParm = -1 Then Return 

IF wf_actualiza_db() THEN
	wf_grabaimagenes()
	
	w_main.SetMicroHelp("Información Grabada.")
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_reportesarribo
integer x = 69
integer y = 64
integer width = 2725
integer height = 284
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_reportesarribo
integer x = 3017
integer y = 408
integer taborder = 60
end type

event pb_nuevo::clicked;call super::clicked;pb_lectura.Enabled	= True
pb_insertar.Enabled	= False
pb_eliminar.Enabled	= False
pb_grabar.Enabled		= False
pb_imprimir.Enabled	= False

uo_SelTipo.Bloquear(False)
uo_SelNaves.Bloquear(False)
uo_SelNaves.LimpiarDatos()
em_Fecha.Enabled = True
em_Fecha.Text		= ''
em_Semana.Text		= ''

il_fila					= 0

end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_reportesarribo
integer x = 3017
integer y = 132
integer taborder = 50
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_reportesarribo
integer x = 3017
integer y = 792
integer taborder = 90
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_reportesarribo
integer x = 3017
integer y = 612
integer taborder = 80
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_reportesarribo
integer x = 3017
integer y = 1536
integer taborder = 120
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_reportesarribo
integer x = 3017
integer y = 1152
integer taborder = 110
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_reportesarribo
integer x = 3017
integer y = 972
integer taborder = 100
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_reportesarribo
integer y = 380
integer width = 2725
integer height = 1572
integer taborder = 70
string dataobject = "dw_mues_reportesarribo"
boolean hscrollbar = true
end type

event dw_1::itemchanged;call super::itemchanged;Long		li_Null
String   	ls_Columna

SetNull(li_Null)

ls_Columna = dwo.Name

Choose Case ls_Columna
	Case "rear_numero"
		If Duplicado(ls_Columna, Data) Then
			This.SetItem(Row, ls_Columna, li_Null)
			Return 1
		End If
		
	Case 'reci_codigo'
		If Not iuo_Recibidor.Existe(Long(Data), True, Sqlca) Then
			This.SetItem(Row, ls_Columna, li_Null)
			Return 1
		End If
		
	Case 'vari_codigo'
		If Not iuo_Variedad.Existe(ii_Especie, Long(Data), True, Sqlca) Then
			This.SetItem(Row, ls_Columna, li_Null)
			Return 1
		End If
		
	Case 'prod_codigo'
		If Not iuo_Productor.Existe(Long(Data), True, Sqlca) Then
			This.SetItem(Row, ls_Columna, li_Null)
			Return 1
		Else
			This.Object.zona_codigo[Row] = iuo_Productor.Zona
		End If

End Choose 

end event

event dw_1::buttonclicked;call super::buttonclicked;String   	ls_Boton, ls_Ruta, ls_Archivo, ls_Filtro

ls_Boton = dwo.Name

Choose Case ls_Boton
	Case "b_carga"
		ls_Filtro	= 'Word Files (*.doc),*.doc,Excel Files (*.xls),*.xls,Acrobat Files (*.pdf),*.pdf,Text Files (*.txt),*.txt, All Files (*.*),*.*'
		If iuo_Doctos.ObtieneArchivo(ls_Ruta, ls_Archivo, ls_Filtro) Then
			This.Object.rear_rutas[Row]		=	Mid(ls_Ruta, 1, LastPos(ls_Ruta, '\'))
			This.Object.rear_archiv[Row]	=	ls_Archivo
		Else
			MessageBox('Atencion', 'No se pudo recuperar archivo.')
			Return 1
		End If
		
	Case "b_ver"
		iuo_Doctos.RecuperaImagen(dw_1, Row, sqlca)

End Choose 
end event

event dw_1::sqlpreview;//
end event

type uo_seltipo from uo_seleccion_tipotransporte within w_mant_mues_reportesarribo
integer x = 635
integer y = 108
integer height = 84
integer taborder = 40
boolean bringtotop = true
end type

on uo_seltipo.destroy
call uo_seleccion_tipotransporte::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case '*', '**'
		
	Case Else
		uo_SelNaves.Filtra(This.Codigo)
		
End Choose
end event

type uo_selnaves from uo_seleccion_naves within w_mant_mues_reportesarribo
integer x = 635
integer y = 216
integer height = 84
integer taborder = 50
boolean bringtotop = true
end type

on uo_selnaves.destroy
call uo_seleccion_naves::destroy
end on

type st_1 from statictext within w_mant_mues_reportesarribo
integer x = 1646
integer y = 228
integer width = 434
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Semana liq"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_2 from statictext within w_mant_mues_reportesarribo
integer x = 1646
integer y = 120
integer width = 434
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Fecha zarpe"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_3 from statictext within w_mant_mues_reportesarribo
integer x = 151
integer y = 120
integer width = 434
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Tipo Trasporte"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_4 from statictext within w_mant_mues_reportesarribo
integer x = 151
integer y = 228
integer width = 434
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Nave"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_fecha from editmask within w_mant_mues_reportesarribo
integer x = 2121
integer y = 108
integer width = 402
integer height = 88
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

event modified;em_Semana.Text = String(F_NroSemana(Datetime(This.Text), 1))
end event

type em_semana from editmask within w_mant_mues_reportesarribo
integer x = 2112
integer y = 216
integer width = 402
integer height = 88
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "##"
end type

