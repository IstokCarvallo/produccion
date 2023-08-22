$PBExportHeader$w_mant_mues_controldespachos.srw
forward
global type w_mant_mues_controldespachos from w_mant_tabla
end type
type st_1 from statictext within w_mant_mues_controldespachos
end type
type st_2 from statictext within w_mant_mues_controldespachos
end type
type uo_selcliente from uo_seleccion_clientesprod within w_mant_mues_controldespachos
end type
type uo_selplanta from uo_seleccion_plantas within w_mant_mues_controldespachos
end type
end forward

global type w_mant_mues_controldespachos from w_mant_tabla
integer width = 3488
string title = "CONTROL DESPACHOS"
event ue_validapassword ( )
st_1 st_1
st_2 st_2
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
end type
global w_mant_mues_controldespachos w_mant_mues_controldespachos

type variables
w_mant_deta_controldespachos  iw_mantencion

end variables

forward prototypes
public function datetime fechahora ()
public subroutine actualizadespacho ()
protected function boolean wf_actualiza_db ()
end prototypes

event ue_validapassword();Str_mant					lstr_mant

lstr_mant.Argumento[1]	=	"Produccion"
lstr_mant.Argumento[2]	=	gs_clavecomext+'comext'

OpenWithParm(w_password, lstr_mant)

lstr_mant	=	Message.PowerObjectParm

IF lstr_mant.Respuesta = 0 THEN
	Close(This)
	return
END IF	

istr_mant.argumento[1]	=	String(gi_codexport)
istr_mant.argumento[2]	=	String(gi_codplanta)
end event

public function datetime fechahora ();Datetime ldt_FechaHora
Time		lt_Hora
Integer	li_Contador

IF sqlca.Dbms = "ODBC" THEN
	SELECT	Count(*), GetDate()
		INTO	:li_Contador, :ldt_FechaHora
		FROM	dbo.admasistemas;	
ELSE
	SELECT	Count(*), GetDate()
		INTO	:li_Contador, :ldt_FechaHora
		FROM	dbo.admasistemas;
END IF

RETURN ldt_FechaHora
end function

public subroutine actualizadespacho ();Integer li_cliente, li_planta, li_nulo
Long	ll_numero, ll_fila
String ls_usuario
Date ld_fecha
Time lt_hora

SetNull(li_nulo)

FOR ll_fila = 1 TO dw_1.RowCount()
	IF dw_1.GetItemStatus(ll_fila,'code_motivo', Primary!)= DataModified!THEN
		li_cliente = dw_1.Object.clie_codigo[ll_fila]
		li_planta  = dw_1.Object.plde_codigo[ll_fila]
		ll_numero  = dw_1.Object.defe_numero[ll_fila]
		ls_usuario = dw_1.Object.usua_codigo[ll_fila]
		ld_fecha   = dw_1.Object.code_fechaa[ll_fila]
		lt_hora    = dw_1.Object.code_horaap[ll_fila]
		
		UPDATE dbo.DESPAFRIGOEN SET
		defe_estado = 0 WHERE
		clie_codigo = :li_cliente AND
		plde_codigo = :li_planta AND
		defe_numero = :ll_numero;
		
		UPDATE dbo.controldespachos SET
		code_gemsaa = NULL,
		code_gengde = NULL,
		//code_gengde = NULL,
		code_demade = NULL 
		WHERE
		clie_codigo = :li_cliente AND
		plde_codigo = :li_planta AND
		defe_numero = :ll_numero;
	END IF	
NEXT	
end subroutine

protected function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_1.uf_check_required(0) THEN RETURN False
IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF dw_1.Update(True, False) = 1 then 
	Commit;
	
	IF sqlca.SQLCode <> 0 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		lb_Retorno	=	False
	ELSE
		lb_Retorno	=	True
		ActualizaDespacho()	
		dw_1.ResetUpdate()
	END IF
ELSE
	RollBack;
	
	IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
	
	lb_Retorno	=	False
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

on w_mant_mues_controldespachos.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.uo_selcliente
this.Control[iCurrent+4]=this.uo_selplanta
end on

on w_mant_mues_controldespachos.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
end on

event ue_borrar;IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra	= True
istr_mant.agrega	= False

istr_mant.argumento[1]	=	String(uo_SelCliente.Codigo)
istr_mant.argumento[2]	=	String(uo_SelPlanta.Codigo)

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

event open;call super::open;String ls_usurio
Time lt_hora
Date ld_fecha
Boolean	lb_Cerrar

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(False, False)
	uo_SelPlanta.Filtra(1)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlanta.Inicia(gi_CodPlanta)
	
	ls_usurio  = gstr_us.nombre
	ld_fecha = date(now()) 
	lt_hora = Time(fechahora())
	
	istr_mant.argumento[3] = ls_usurio
	istr_mant.argumento[5] = String(ld_fecha)
	istr_mant.argumento[6] = String(lt_hora)
								
	PostEvent("ue_validapassword")
End If
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta

DO
	ll_fila	= dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo)
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SelectRow(1,True)
		dw_1.SetFocus()
		pb_imprimir.Enabled	= True
		//pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
	ELSE
		pb_insertar.SetFocus()
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_modifica;call super::ue_modifica;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False
	
	istr_mant.argumento[1]	=	String(uo_SelCliente.Codigo)
	istr_mant.argumento[2]	=	String(uo_SelPlanta.Codigo)
	istr_mant.argumento[5] = String(dw_1.Object.code_fechaa[il_fila])
	istr_mant.argumento[6] = String(dw_1.Object.code_horaap[il_fila])
	
	OpenWithParm(iw_mantencion, istr_mant)
END IF

end event

event ue_nuevo;call super::ue_nuevo;Date ld_fecha
Time lt_hora

istr_mant.borra	= False
istr_mant.agrega	= True
	
ld_fecha = date(now()) 
lt_hora = Time(fechahora())

istr_mant.argumento[1]	=	String(uo_SelCliente.Codigo)
istr_mant.argumento[2]	=	String(uo_SelPlanta.Codigo)
istr_mant.argumento[5] = String(ld_fecha)
istr_mant.argumento[6] = String(lt_hora)

OpenWithParm(iw_mantencion, istr_mant)
	
IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
	//pb_eliminar.Enabled	= TRUE
	pb_grabar.Enabled		= TRUE
END IF
	
dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)

end event

event resize;call super::resize;//
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila

istr_info.titulo	= "CONTROL DE DESPACHOS"
istr_info.copias	= 1

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_control_despachos"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

event ue_listo;IF dw_1.RowCount() > 0 THEN
	pb_Imprimir.Enabled	=	True
	
	IF istr_mant.Solo_Consulta THEN
		pb_Eliminar.Enabled	=	False
		pb_Grabar.Enabled		=	False
		pb_Insertar.Enabled	=	False
	ELSE
		//pb_Eliminar.Enabled	=	True
		pb_Grabar.Enabled		=	True
		pb_Insertar.Enabled	=	True
	END IF
ELSE
	IF istr_mant.Solo_Consulta THEN
		pb_Insertar.Enabled	=	False
	ELSE
		pb_Insertar.Enabled	=	True
	END IF
END IF

w_main.SetMicroHelp("Listo")

SetPointer(Arrow!)
end event

event ue_antesguardar;Long ll_fila

FOR ll_fila = 1 TO dw_1.RowCount()
	IF dw_1.GetItemStatus(ll_fila,'code_motivo', Primary!)= DataModified!THEN
		dw_1.Object.code_gemsaa[ll_fila] = 0
		dw_1.Object.code_gengde[ll_fila] = 0
		dw_1.Object.code_gemide[ll_fila] = 0
		dw_1.Object.code_demade[ll_fila] = 0 
	END IF
NEXT	
end event

event ue_guardar;call super::ue_guardar;TriggerEvent("ue_recuperadatos")
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_controldespachos
integer x = 82
integer y = 360
integer width = 2875
integer height = 1324
string dataobject = "dw_mues_controldespachos"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_controldespachos
integer x = 82
integer y = 40
integer width = 2875
integer height = 284
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_controldespachos
integer x = 3058
integer y = 108
integer taborder = 20
end type

event pb_lectura::clicked;call super::clicked;uo_SelCliente.Bloquear(True)
uo_SelPlanta.Bloquear(True)
end event

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_controldespachos
integer x = 3058
integer y = 408
integer taborder = 40
end type

event pb_nuevo::clicked;call super::clicked;uo_SelCliente.Bloquear(False)
uo_SelPlanta.Bloquear(False)
end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_controldespachos
boolean visible = false
integer x = 3058
integer y = 660
integer taborder = 50
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_controldespachos
boolean visible = false
integer x = 3058
integer y = 912
integer taborder = 60
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_controldespachos
integer x = 3058
integer y = 1168
integer taborder = 70
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_controldespachos
integer x = 3058
integer y = 1420
integer taborder = 80
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_controldespachos
integer x = 3246
integer taborder = 90
end type

type st_1 from statictext within w_mant_mues_controldespachos
integer x = 1691
integer y = 152
integer width = 247
integer height = 76
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
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within w_mant_mues_controldespachos
integer x = 174
integer y = 152
integer width = 247
integer height = 76
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
string text = "Cliente"
boolean focusrectangle = false
end type

type uo_selcliente from uo_seleccion_clientesprod within w_mant_mues_controldespachos
event destroy ( )
integer x = 453
integer y = 140
integer height = 96
integer taborder = 30
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_mant_mues_controldespachos
event destroy ( )
integer x = 1970
integer y = 140
integer height = 96
integer taborder = 30
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

