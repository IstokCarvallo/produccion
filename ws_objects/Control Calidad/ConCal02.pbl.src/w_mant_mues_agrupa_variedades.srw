$PBExportHeader$w_mant_mues_agrupa_variedades.srw
$PBExportComments$Mantenedor de Daños por Especie.
forward
global type w_mant_mues_agrupa_variedades from w_mant_tabla
end type
type st_1 from statictext within w_mant_mues_agrupa_variedades
end type
type st_2 from statictext within w_mant_mues_agrupa_variedades
end type
type uo_selespecie from uo_seleccion_especie within w_mant_mues_agrupa_variedades
end type
type cb_agrupa from commandbutton within w_mant_mues_agrupa_variedades
end type
type sle_agrupa from singlelineedit within w_mant_mues_agrupa_variedades
end type
type em_agrupa from editmask within w_mant_mues_agrupa_variedades
end type
end forward

global type w_mant_mues_agrupa_variedades from w_mant_tabla
integer width = 2226
integer height = 1912
string title = "Maestro Agrupacion Variedades"
st_1 st_1
st_2 st_2
uo_selespecie uo_selespecie
cb_agrupa cb_agrupa
sle_agrupa sle_agrupa
em_agrupa em_agrupa
end type
global w_mant_mues_agrupa_variedades w_mant_mues_agrupa_variedades

type variables
w_mant_deta_agrupa_variedades  iw_mantencion

Long		li_Especie

DatawindowChild	idwc_Variedades
end variables

forward prototypes
public function string wf_nombreagrupa (integer ai_especie, integer ai_grupo)
end prototypes

public function string wf_nombreagrupa (integer ai_especie, integer ai_grupo);String	ls_Retorno

Select IsNull(Max(agva_nombre), '')
	Into :ls_retorno
    From dbo.ctlcal_agrupavariedad
   Where espe_codigo = :ai_Especie
	  And agva_codigo = :ai_grupo
	  Using SqlCa;
	  
IF sqlca.SQLCode <> 0 THEN
	MessageBox('Error', 'No se pudo conectar a la Base de Datos', StopSign!, Ok!)
	ls_retorno = ''
End If	  

Return ls_Retorno
end function

on w_mant_mues_agrupa_variedades.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.uo_selespecie=create uo_selespecie
this.cb_agrupa=create cb_agrupa
this.sle_agrupa=create sle_agrupa
this.em_agrupa=create em_agrupa
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.uo_selespecie
this.Control[iCurrent+4]=this.cb_agrupa
this.Control[iCurrent+5]=this.sle_agrupa
this.Control[iCurrent+6]=this.em_agrupa
end on

on w_mant_mues_agrupa_variedades.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.uo_selespecie)
destroy(this.cb_agrupa)
destroy(this.sle_agrupa)
destroy(this.em_agrupa)
end on

event open;call super::open;Boolean	lb_Cerrar

If IsNull(uo_SelEspecie.Codigo)		Then lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else	
	li_Especie = Message.DoubleParm
	
	uo_SelEspecie.Seleccion(False, False)
	
	If li_Especie <> 0 Then
		uo_SelEspecie.Bloquear(True)
		uo_SelEspecie.Codigo	=	li_Especie
		uo_SelEspecie.dw_Seleccion.Object.Codigo[1]=	li_Especie
	End If
	
	istr_mant.dw	= dw_1
	
	dw_1.GetChild('vari_codigo', idwc_Variedades)
	idwc_Variedades.SetTransObject(Sqlca)
	idwc_Variedades.Retrieve(li_Especie)
	
	istr_mant.argumento[1]	=	String(gi_CodExport)
	istr_mant.argumento[2]	=	"0"
	
	buscar	=	"Orden:Nccdc_ordena,Calibres:Sccdc_calibr,Gramos:Sccdc_gramos,Milimetros:Scccdc_milime"
	ordenar	=	"Orden:Nccdc_ordena,Calibres:ccdc_calibr,Gramos:ccdc_gramos,Milimetros:cccdc_milim"
End If
end event

event ue_recuperadatos;Long		ll_fila, respuesta

DO	
	ll_fila			=	dw_1.Retrieve(uo_SelEspecie.Codigo, Long(em_Agrupa.Text))
		
	If ll_fila 		=	-1 Then
		respuesta 	=	MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ElseIf ll_fila >= 0 Then
			dw_1.SetFocus()
			il_fila					= 1
			pb_imprimir.Enabled	= True
			pb_insertar.Enabled	= True
			pb_eliminar.Enabled	= True
			pb_grabar.Enabled		= True
	End If
	
LOOP WHILE respuesta = 1

If respuesta = 2 Then Close(This)
end event

event ue_borrar();IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra	= True
istr_mant.agrega	= False

OpenWithParm(iw_mantencion, istr_mant) 

istr_mant = Message.PowerObjectParm

IF istr_mant.respuesta = 1 THEN
	IF dw_1.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF
	
	IF dw_1.RowCount() = 0 THEN
		pb_eliminar.Enabled = False
	ELSE
		il_fila = dw_1.GetRow()
		dw_1.SelectRow(il_fila,True)
	END IF
END IF

istr_mant.borra	 = False
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "MAESTRO AGRUPACION VARIEDADES"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)
vinf.dw_1.DataObject = ""
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(uo_SelEspecie.Codigo, Long(em_Agrupa.Text))

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

event ue_modifica();call super::ue_modifica;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False

	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

event ue_nuevo;istr_mant.borra	= False
istr_mant.agrega	= True

istr_Mant.Argumento[2] = String(uo_SelEspecie.Codigo)
istr_Mant.Argumento[3] = em_Agrupa.Text
istr_Mant.Argumento[4] = sle_Agrupa.Text

OpenWithParm(iw_mantencion, istr_mant)

IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
	pb_eliminar.Enabled	= TRUE
	pb_grabar.Enabled		= TRUE
END IF

dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)

end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_agrupa_variedades
integer y = 524
integer width = 1563
integer height = 1208
integer taborder = 60
string dataobject = "dw_mues_agrupa_variedades"
boolean hscrollbar = true
end type

event dw_1::rowfocuschanged;Integer li_Fila

li_Fila = dw_1.RowCount()

IF RowCount() < 1 OR GetRow() = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila				=	GetRow()
	pb_grabar.Enabled	=	True
END IF
end event

event dw_1::sqlpreview;//
end event

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_agrupa_variedades
integer y = 72
integer width = 1627
integer height = 380
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_agrupa_variedades
integer x = 1824
integer y = 140
end type

event pb_lectura::clicked;call super::clicked;uo_SelEspecie.Bloquear(True)

end event

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_agrupa_variedades
integer x = 1819
integer y = 436
integer taborder = 30
end type

event pb_nuevo::clicked;call super::clicked;If li_Especie = 0 Then
	uo_SelEspecie.Bloquear(False)
	uo_SelEspecie.LimpiarDatos()
End If

em_agrupa.Text = ''
sle_agrupa.Text = ''

pb_insertar.Enabled	= False
pb_eliminar.Enabled	= False
pb_grabar.Enabled		= False
pb_imprimir.Enabled	= False

il_fila					= 0


end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_agrupa_variedades
integer x = 1819
integer y = 616
integer taborder = 80
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_agrupa_variedades
integer x = 1819
integer y = 796
integer taborder = 90
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_agrupa_variedades
integer x = 1819
integer y = 976
integer taborder = 100
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_agrupa_variedades
integer x = 1819
integer y = 1156
integer taborder = 110
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_agrupa_variedades
integer x = 1819
integer y = 1540
integer taborder = 120
end type

type st_1 from statictext within w_mant_mues_agrupa_variedades
integer x = 160
integer y = 152
integer width = 347
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Especie"
boolean focusrectangle = false
end type

type st_2 from statictext within w_mant_mues_agrupa_variedades
integer x = 160
integer y = 308
integer width = 347
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Agrupacion"
boolean focusrectangle = false
end type

type uo_selespecie from uo_seleccion_especie within w_mant_mues_agrupa_variedades
event destroy ( )
integer x = 553
integer y = 136
integer height = 80
integer taborder = 40
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type cb_agrupa from commandbutton within w_mant_mues_agrupa_variedades
integer x = 768
integer y = 304
integer width = 114
integer height = 88
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "..."
end type

event clicked;
istr_busq.Argum[2]	=	String(uo_SelEspecie.Codigo)

OpenWithParm(w_busc_agrupavariedades, istr_busq)

istr_busq = Message.PowerObjectParm

If istr_busq.argum[3] <> "" Then
	em_Agrupa.Text = istr_busq.argum[2]
	sle_Agrupa.Text = istr_busq.argum[3]
	sle_Agrupa.Enabled	= False
	Parent.TriggerEvent("ue_recuperadatos")
End If
	
		
		
end event

type sle_agrupa from singlelineedit within w_mant_mues_agrupa_variedades
integer x = 891
integer y = 300
integer width = 709
integer height = 96
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

type em_agrupa from editmask within w_mant_mues_agrupa_variedades
integer x = 526
integer y = 300
integer width = 233
integer height = 96
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "###"
end type

event modified;If IsNull(This.Text) Then Return

String ls_Nombre

ls_Nombre = wf_NombreAgrupa(uo_SelEspecie.Codigo, Long(This.Text))

If ls_Nombre = '' Then 
	sle_Agrupa.Enabled	= True
	sle_Agrupa.Text 		= ''
	dw_1.Reset()
Else
	sle_Agrupa.Enabled	= False
	sle_Agrupa.Text 		= ls_Nombre
	Parent.TriggerEvent('ue_recuperadatos')
End If


end event

