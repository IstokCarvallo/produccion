$PBExportHeader$w_cons_bins.srw
forward
global type w_cons_bins from w_mant_directo
end type
type st_2 from statictext within w_cons_bins
end type
type em_numero from editmask within w_cons_bins
end type
end forward

global type w_cons_bins from w_mant_directo
integer width = 3086
integer height = 2236
string title = "Consulta de Equipos / Leasing"
st_2 st_2
em_numero em_numero
end type
global w_cons_bins w_cons_bins

type variables
//uo_Equipo	iuo_Equipo
end variables

on w_cons_bins.create
int iCurrent
call super::create
this.st_2=create st_2
this.em_numero=create em_numero
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_2
this.Control[iCurrent+2]=this.em_numero
end on

on w_cons_bins.destroy
call super::destroy
destroy(this.st_2)
destroy(this.em_numero)
end on

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta

DO
	ll_fila	= dw_1.Retrieve(Long(em_numero.Text))
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ELSEIF ll_fila = 1 THEN
		dw_1.SetRow(1)
		dw_1.SetFocus()	
		il_fila	= 1
	ELSE
		MessageBox("Atencion...", "No es posible encontrar codigo ingresado.", Information!, OK!)
		em_numero.Text = ''
		dw_1.InsertRow(0)
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event open;x				=	0
y				=	0
This.Width	=	dw_1.width + 540
This.Height	=	2470	
im_menu		=	m_principal

IF gs_Ambiente = "Windows" THEN
	This.ParentWindow().ToolBarVisible	=	True
	im_menu.Item[1].Item[6].Enabled		=	True
	im_menu.Item[7].Visible					=	False
END IF

This.Icon									=	Gstr_apl.Icono

dw_1.SetTransObject(sqlca)

istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.Solo_Consulta			=	istr_mant.UsuarioSoloConsulta

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "CONSULTA BINS", 1)
end event

type st_encabe from w_mant_directo`st_encabe within w_cons_bins
integer x = 110
integer y = 36
integer width = 2318
integer height = 204
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_cons_bins
boolean visible = false
integer x = 2629
integer y = 316
integer taborder = 50
boolean enabled = false
end type

type pb_lectura from w_mant_directo`pb_lectura within w_cons_bins
boolean visible = false
integer x = 2619
integer y = 80
integer taborder = 40
boolean enabled = false
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_cons_bins
boolean visible = false
integer x = 2619
integer y = 672
integer taborder = 80
end type

type pb_insertar from w_mant_directo`pb_insertar within w_cons_bins
boolean visible = false
integer x = 2633
integer y = 496
integer taborder = 70
end type

type pb_salir from w_mant_directo`pb_salir within w_cons_bins
integer x = 2565
integer y = 1176
integer taborder = 110
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_cons_bins
boolean visible = false
integer x = 2619
integer y = 1032
integer taborder = 100
end type

type pb_grabar from w_mant_directo`pb_grabar within w_cons_bins
boolean visible = false
integer x = 2619
integer y = 852
integer taborder = 90
end type

event pb_grabar::clicked;
//If iuo_usuario.Administra <> 1 Then
//	MessageBox('Atención...', 'No es Administrador no puede cambiar Estado de Solicitudes Solo Consultar', &
//					StopSign!, OK!)
//	Return -1
//End If

Call Super::clicked
end event

type dw_1 from w_mant_directo`dw_1 within w_cons_bins
integer x = 110
integer y = 272
integer width = 2318
integer height = 1752
integer taborder = 60
string dataobject = "dw_cons_bins"
boolean livescroll = false
end type

event dw_1::clicked;call super::clicked;String	ls_old_sort, ls_column
Char		lc_sort

IF IsNull(dwo) THEN RETURN

If Right(dwo.Name,2) = '_t' Then
	ls_column = Left (dwo.Name, Len(String(dwo.Name)) - 2)
	ls_old_sort = This.Describe("Datawindow.Table.sort")

	If ls_column = Left(ls_old_sort, Len(ls_old_sort) - 2) Then
		lc_sort = Right(ls_old_sort, 1)
		If lc_sort = 'A' Then
			lc_sort = 'D'
		Else
			lc_sort = 'A'
		End If
		This.SetSort(ls_column+" "+lc_sort)
	Else
		This.SetSort(ls_column+" A")
		This.Modify(Left(ls_old_sort, Len(ls_old_sort) - 2) + "_t.Color = 0")
	End If
	
	This.Modify(dwo.Name + ".Color = " + String(Rgb(0, 0, 255)))
	
	This.Sort()
End If

IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

type st_2 from statictext within w_cons_bins
integer x = 151
integer y = 116
integer width = 361
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
long textcolor = 16777215
long backcolor = 553648127
string text = "Nro. BINS"
boolean focusrectangle = false
end type

type em_numero from editmask within w_cons_bins
integer x = 539
integer y = 92
integer width = 1179
integer height = 112
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

event modified;Parent.PostEvent("ue_recuperadatos")


end event

