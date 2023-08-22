$PBExportHeader$w_mant_mues_usuariosweb.srw
forward
global type w_mant_mues_usuariosweb from w_mant_tabla
end type
type st_1 from statictext within w_mant_mues_usuariosweb
end type
type uo_selusuario from uo_seleccion_admausuarios within w_mant_mues_usuariosweb
end type
end forward

global type w_mant_mues_usuariosweb from w_mant_tabla
integer width = 3415
string title = "ACCESO USUARIOS WEB"
st_1 st_1
uo_selusuario uo_selusuario
end type
global w_mant_mues_usuariosweb w_mant_mues_usuariosweb

type variables
w_mant_deta_usuariosweb  iw_mantencion
end variables

event ue_nuevo;istr_mant.borra	= False
istr_mant.agrega	= True

istr_mant.Argumento[1] = String(uo_SelUsuario.Codigo)

OpenWithParm(iw_mantencion, istr_mant)

IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
	pb_eliminar.Enabled	= TRUE
	pb_grabar.Enabled		= TRUE
END IF

dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila

istr_info.titulo	= "MAESTRO ACCESOS USUARIOS WEB"
istr_info.copias	= 1

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_web_usuariosproductor"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(uo_SelUsuario.Codigo)

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)


end event

event ue_recuperadatos;Long	ll_fila, respuesta

DO
	ll_fila	= dw_1.Retrieve(uo_SelUsuario.Codigo)
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SelectRow(1,True)
		dw_1.SetFocus()
		pb_nuevo.Enabled		= True
				
		IF istr_mant.Solo_Consulta <> True THEN
			pb_insertar.Enabled	= True
			pb_eliminar.Enabled	= True
			pb_grabar.Enabled		= True
		END IF	
			pb_imprimir.Enabled	= True
	ELSE
		IF istr_mant.Solo_Consulta <> True THEN	
			pb_insertar.Enabled	= True
			pb_nuevo.Enabled		= True
			pb_insertar.SetFocus()
		END IF	
		pb_imprimir.Enabled	= True
	END IF

LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

on w_mant_mues_usuariosweb.create
int iCurrent
call super::create
this.st_1=create st_1
this.uo_selusuario=create uo_selusuario
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.uo_selusuario
end on

on w_mant_mues_usuariosweb.destroy
call super::destroy
destroy(this.st_1)
destroy(this.uo_selusuario)
end on

event ue_borrar;IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.Borra		= True
istr_mant.Agrega	= False
istr_mant.Argumento[1] = String(uo_SelUsuario.Codigo)

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

event open;call super::open;Boolean	lb_Cerrar = False

If IsNull(uo_SelUsuario.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelUsuario.Seleccion(False, False)
		
	buscar	= "Productor:Nprod_codigo,Predio:Nprbr_codpre"
	ordenar	= "Productor:prod_codigo,Predio:prbr_codpre"
End If
end event

event ue_modifica;IF dw_1.RowCount() > 0 THEN
	istr_mant.Agrega	= False
	istr_mant.Borra		= False
	
	istr_mant.Argumento[1] = String(uo_SelUsuario.Codigo)

	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_usuariosweb
integer x = 82
integer y = 388
integer width = 2528
integer height = 1188
string dataobject = "dw_mues_web_usuariosproductor"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_usuariosweb
integer x = 82
integer y = 100
integer width = 2528
integer height = 272
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_usuariosweb
integer x = 2862
integer y = 84
end type

event pb_lectura::clicked;call super::clicked;uo_SelUsuario.Bloquear(True)
end event

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_usuariosweb
integer x = 2862
integer y = 376
boolean enabled = false
end type

event pb_nuevo::clicked;call super::clicked;uo_SelUsuario.Bloquear(False)
end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_usuariosweb
integer x = 2862
integer y = 524
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_usuariosweb
integer x = 2862
integer y = 752
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_usuariosweb
integer x = 2862
integer y = 968
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_usuariosweb
integer x = 2862
integer y = 1184
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_usuariosweb
integer x = 2862
integer y = 1400
integer height = 228
end type

type st_1 from statictext within w_mant_mues_usuariosweb
integer x = 594
integer y = 200
integer width = 247
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Usuario"
boolean focusrectangle = false
end type

type uo_selusuario from uo_seleccion_admausuarios within w_mant_mues_usuariosweb
event destroy ( )
integer x = 987
integer y = 192
integer height = 88
integer taborder = 20
boolean bringtotop = true
end type

on uo_selusuario.destroy
call uo_seleccion_admausuarios::destroy
end on

