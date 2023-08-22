$PBExportHeader$w_proc_rechazo_lote_sag.srw
$PBExportComments$Genera Archivo Plano SAG de Eliminación de Pallets.
forward
global type w_proc_rechazo_lote_sag from w_mant_tabla
end type
type st_3 from statictext within w_proc_rechazo_lote_sag
end type
type dw_cliente from datawindow within w_proc_rechazo_lote_sag
end type
type st_4 from statictext within w_proc_rechazo_lote_sag
end type
type dw_planta from datawindow within w_proc_rechazo_lote_sag
end type
type st_numero from statictext within w_proc_rechazo_lote_sag
end type
type em_nrosag from editmask within w_proc_rechazo_lote_sag
end type
type em_fecha from editmask within w_proc_rechazo_lote_sag
end type
type st_2 from statictext within w_proc_rechazo_lote_sag
end type
type dw_2 from datawindow within w_proc_rechazo_lote_sag
end type
type rb_1 from radiobutton within w_proc_rechazo_lote_sag
end type
type rb_2 from radiobutton within w_proc_rechazo_lote_sag
end type
type cbx_1 from checkbox within w_proc_rechazo_lote_sag
end type
type rb_3 from radiobutton within w_proc_rechazo_lote_sag
end type
type rb_borrar from radiobutton within w_proc_rechazo_lote_sag
end type
end forward

global type w_proc_rechazo_lote_sag from w_mant_tabla
integer width = 3666
integer height = 1932
string title = "APROBACIÓN, RECHAZO U OBJECIÓN LOTES SAG"
event ue_validapassword ( )
st_3 st_3
dw_cliente dw_cliente
st_4 st_4
dw_planta dw_planta
st_numero st_numero
em_nrosag em_nrosag
em_fecha em_fecha
st_2 st_2
dw_2 dw_2
rb_1 rb_1
rb_2 rb_2
cbx_1 cbx_1
rb_3 rb_3
rb_borrar rb_borrar
end type
global w_proc_rechazo_lote_sag w_proc_rechazo_lote_sag

type variables
Integer			ii_PlantaSag
Date				id_fecha

w_gene_deta_archivo_eliminacion	iw_mantencion

DataWindowChild	idwc_cliente, idwc_planta
end variables

forward prototypes
public function integer codigoplantasag (integer planta)
public function boolean noexistecliente (integer ai_cliente)
public function boolean noexisteplanta (integer ai_planta)
protected function boolean wf_actualiza_db ()
public function boolean existeinspeccion (integer ai_cliente, integer ai_planta, long al_numero)
public subroutine borra_inspecciones (integer ai_planta, integer ai_cliente, integer al_numero)
end prototypes

event ue_validapassword();Str_mant					lstr_mant

lstr_mant.Argumento[1]	=	"Produccion"
lstr_mant.Argumento[2]	=	gs_Password+''+'contraparte'

OpenWithParm(w_password, lstr_mant)

lstr_mant	=	Message.PowerObjectParm

end event

public function integer codigoplantasag (integer planta);Integer	li_Cliente, li_codigo

li_Cliente	=	Integer(istr_mant.Argumento[1])

SELECT	plde_codsag
	INTO	:li_Codigo
	FROM	dbo.plantadesp
	WHERE	plde_codigo	=	:planta;

RETURN li_Codigo

end function

public function boolean noexistecliente (integer ai_cliente);String	ls_Nombre

SELECT	clie_nombre
	INTO	:ls_Nombre
	FROM	dbo.CLIENTESPROD
	WHERE	:ai_Cliente in (-1,clie_codigo);
			
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Clientes Producción")
	
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Cliente no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.", Exclamation!, Ok!)
					
	RETURN True
ELSE

	istr_mant.argumento[1]	=	String(ai_cliente)
	RETURN False
END IF
end function

public function boolean noexisteplanta (integer ai_planta);String	ls_Nombre 
Integer  li_Cliente

li_Cliente	=	Integer(istr_mant.argumento[1])

SELECT	plde_nombre, plde_codsag
	INTO	:ls_Nombre, :ii_PlantaSag
	FROM	dbo.PLANTADESP
	WHERE	plde_codigo	=	:ai_Planta;
			
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Plantas y Frigoríficos")
	
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Planta no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.", Exclamation!, Ok!)
	
	RETURN True
ELSE
	RETURN False
END IF
end function

protected function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno

//IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

	IF dw_2.Update(True, False) = 1 THEN
		IF dw_1.Update(True, False) = 1 THEN
			Commit;
			
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			ELSE
				lb_Retorno	=	True
				
				dw_1.ResetUpdate()
				dw_2.ResetUpdate()
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
	END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public function boolean existeinspeccion (integer ai_cliente, integer ai_planta, long al_numero);Date	ld_fecha
Long	ll_cuenta

SELECT	count(*) 
	INTO	:ll_cuenta
	FROM	dbo.inspecpalenc
	WHERE	:ai_Cliente in (-1,clie_codigo)
	AND	plde_codigo = :ai_planta
	AND	inpe_numero = :al_numero
	AND	inpe_estado = 5;
			
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla inspecpalenc")
	
	RETURN True
ELSEIF ll_cuenta = 0 THEN
	MessageBox("Atención", "Número de inspección no ha sido Definido o ya se Encuentra en Estado~r Definitivo .~r~r" + &
					"Ingrese o seleccione otro Número.", Exclamation!, Ok!)
					
	RETURN True
ELSE

	RETURN False
END IF
end function

public subroutine borra_inspecciones (integer ai_planta, integer ai_cliente, integer al_numero);DELETE dbo.inspecpaldet 
WHERE :ai_cliente in (-1,clie_codigo)
AND	plde_codigo = :ai_planta
AND	inpe_numero = :al_numero;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla inspecpaldet")
END IF	

DELETE dbo.inspecpalenc
WHERE :ai_cliente in (-1,clie_codigo)
AND	plde_codigo = :ai_planta
AND	inpe_numero = :al_numero;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla inspecpalenc")
END IF	


end subroutine

on w_proc_rechazo_lote_sag.create
int iCurrent
call super::create
this.st_3=create st_3
this.dw_cliente=create dw_cliente
this.st_4=create st_4
this.dw_planta=create dw_planta
this.st_numero=create st_numero
this.em_nrosag=create em_nrosag
this.em_fecha=create em_fecha
this.st_2=create st_2
this.dw_2=create dw_2
this.rb_1=create rb_1
this.rb_2=create rb_2
this.cbx_1=create cbx_1
this.rb_3=create rb_3
this.rb_borrar=create rb_borrar
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_3
this.Control[iCurrent+2]=this.dw_cliente
this.Control[iCurrent+3]=this.st_4
this.Control[iCurrent+4]=this.dw_planta
this.Control[iCurrent+5]=this.st_numero
this.Control[iCurrent+6]=this.em_nrosag
this.Control[iCurrent+7]=this.em_fecha
this.Control[iCurrent+8]=this.st_2
this.Control[iCurrent+9]=this.dw_2
this.Control[iCurrent+10]=this.rb_1
this.Control[iCurrent+11]=this.rb_2
this.Control[iCurrent+12]=this.cbx_1
this.Control[iCurrent+13]=this.rb_3
this.Control[iCurrent+14]=this.rb_borrar
end on

on w_proc_rechazo_lote_sag.destroy
call super::destroy
destroy(this.st_3)
destroy(this.dw_cliente)
destroy(this.st_4)
destroy(this.dw_planta)
destroy(this.st_numero)
destroy(this.em_nrosag)
destroy(this.em_fecha)
destroy(this.st_2)
destroy(this.dw_2)
destroy(this.rb_1)
destroy(this.rb_2)
destroy(this.cbx_1)
destroy(this.rb_3)
destroy(this.rb_borrar)
end on

event open;call super::open;x = 0
y = 0

This.Icon	=	Gstr_apl.Icono

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_cliente.InsertRow(0)
//dw_cliente.SetItem(1,"clie_codigo",0)

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_planta.InsertRow(0)
dw_planta.SetItem(1,"plde_codigo",gi_CodPlanta)

istr_mant.argumento[1] 	= 	'-1'
istr_mant.argumento[2]	=	String(gi_CodPlanta)
istr_mant.argumento[3]	=	'0'
istr_mant.argumento[4]	=	'0'
em_fecha.text = String(Today())

dw_2.SetTransObject(sqlca)

end event

event closequery;//
end event

event ue_recuperadatos;Long	ll_fila, respuesta,ll_fila1

dw_1.Reset()
dw_2.Reset()

DO
	ll_fila	= dw_1.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]), &
	                         Long(istr_mant.argumento[3]))
	
	ll_fila	= dw_2.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]), &
	                         Long(istr_mant.argumento[3]))
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SelectRow(1,True)
		dw_1.SetFocus()
//		pb_imprimir.Enabled	= True
//		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
	ELSE
//		pb_insertar.SetFocus()
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_antesguardar;call super::ue_antesguardar;Integer		li_Cliente, li_Planta, li_tipoin
Long			ll_Fila, ll_Numero, ll_numinp, ll_null
Date			ld_fechai
String		ls_fecha

SetNull(ll_null)

ls_fecha = em_fecha.text

/*	Asegura Rechazo */
//IF MessageBox("Advertencia", "Rechazará el Lote .~r~r" + &
//			"Desea continuar ?", Question!, YesNo!, 2) = 1 THEN 

	FOR ll_Fila = 1 TO dw_1.RowCount()
		
		li_Cliente	=	dw_1.Object.Clie_codigo[ll_Fila]
		li_Planta	=	dw_1.Object.Plde_codigo[ll_Fila]
		ll_Numero	=	dw_1.Object.Paen_numero[ll_Fila]
		ld_fechai	=	Date(ls_fecha)
		li_tipoin	=	dw_1.Object.inpe_tipoin[ll_Fila]
		ll_Numinp	=	dw_1.Object.inpe_numero[ll_Fila]
		
		IF rb_1.Checked THEN
			//aprobado
			dw_1.Object.paen_inspec[ll_Fila]	=	1
			dw_1.Object.inpe_numero[ll_Fila]	=	Long(em_nrosag.Text)
			dw_1.Object.inpe_tipoin[ll_Fila]	=	1
			dw_1.Object.inpe_fechai[ll_Fila]	=  ld_fechai
		ELSEIF rb_2.Checked THEN
			//rechazado
			dw_1.Object.paen_inspec[ll_Fila]	=	3
			//dw_1.Object.inpe_numero[ll_Fila]	=	Long(em_nrosag.Text)
			dw_1.Object.inpe_tipoin[ll_Fila]	=	3
			dw_1.Object.inpe_fechai[ll_Fila]	=	Date('19000101')
			dw_2.Object.inpd_frecha[ll_Fila]	=	ld_fechai
			dw_2.Object.inpd_fhisre[ll_Fila]	=	ld_fechai
			dw_2.Object.inpd_desant[ll_Fila]	=	dw_2.Object.dest_codigo[ll_Fila]
			dw_2.Object.dest_codigo[ll_Fila]	=	999
			dw_1.Object.dest_codigo[ll_Fila]	=	999
		ELSEIF rb_3.Checked THEN	
			//objetado
			dw_1.Object.paen_inspec[ll_Fila]	=	0
			dw_1.Object.inpe_numero[ll_Fila]	=	0
			dw_1.Object.inpe_tipoin[ll_Fila]	=	0
			dw_1.Object.inpe_fechai[ll_Fila]	=  Date('19000101')
		ELSEIF rb_borrar.Checked THEN
			//Borrar
			dw_1.Object.paen_inspec[ll_Fila]	=	0
			dw_1.Object.inpe_numero[ll_Fila]	=	ll_null
			dw_1.Object.inpe_tipoin[ll_Fila]	=	Integer(ll_null)
			dw_1.Object.inpe_fechai[ll_Fila]	=	Date('19000101')
			dw_1.Object.dest_codigo[ll_Fila]	=	999	
		END IF
		
		dw_1.Object.paen_fecter[ll_Fila] = Datetime(Today(), Now())
	NEXT

//END IF
end event

event ue_guardar;Integer	li_cliente, li_planta, li_estado
Long		ll_numero
Date		ld_fecres, ld_fecha

istr_mant.argumento[3] = em_nrosag.Text

li_cliente 	= 	Integer(istr_mant.argumento[1])
li_planta		=	Integer(istr_mant.argumento[2])
ll_numero	=	Long(istr_mant.argumento[3])
ld_fecres		= 	Date(em_fecha.Text)

IF rb_1.Checked THEN
	li_estado = 1
ELSEIF rb_2.Checked THEN
	li_estado = 3
ELSEIF rb_3.Checked THEN 
	li_estado = -2
END IF	

//rb_borrar

IF dw_1.AcceptText() = -1 THEN RETURN
SetPointer(HourGlass!)
w_main.SetMicroHelp("Grabando información...")
Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF rb_2.Checked THEN
	ld_fecha = Date(em_fecha.text)
ELSE
	ld_fecha = Date(19000101)
END IF	

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
	
	IF NOT rb_borrar.Checked THEN
		UPDATE dbo.inspecpalenc SET
			inpe_estado = :li_estado,
			inpe_fecres = :ld_fecres,
			inpe_desant = ins.dest_codigo,
			inpe_fhisre = :ld_fecha,
			inpe_secant = ins.inpe_dessec,
			inpe_detant = ins.inpe_desdet
			FROM dbo.inspecpalenc as ins
			WHERE :li_cliente in (-1,ins.clie_codigo)
			AND	ins.plde_codigo = :li_planta
			AND	ins.inpe_numero = :ll_numero
			AND inpe_numero = ins.inpe_numero
			AND clie_codigo = ins.clie_codigo
			AND plde_codigo = ins.plde_codigo;
		
		commit;
		
		UPDATE dbo.inspecpaldet SET
			inpd_fecres = :ld_fecres
			WHERE :li_cliente in (-1,clie_codigo)
			AND	plde_codigo = :li_planta
			AND	inpe_numero = :ll_numero;
		
		commit;
	ELSE
		
		li_planta = dw_planta.Object.plde_codigo[1]
	
		borra_inspecciones(li_planta,li_cliente,ll_numero)
	END IF
	//pb_imprimir.Enabled	= True
	
	dw_1.Reset()
	dw_2.Reset()
	MessageBox("Atención", "Datos Grabados Exitosamente", Exclamation!, Ok!)
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

type dw_1 from w_mant_tabla`dw_1 within w_proc_rechazo_lote_sag
integer y = 636
integer width = 3031
integer height = 1108
integer taborder = 0
string dataobject = "dw_mues_rechazo_lote_sag"
boolean hscrollbar = true
end type

event dw_1::clicked;call super::clicked;String		ls_Tecla

IF KeyDown(KeyShift!) THEN
	ls_tecla	=	"Shift"
ELSEIF KeyDown(KeyControl!) THEN
	ls_tecla	=	"Control"
END IF

F_Selecciona(This, ls_Tecla, Row)

IF dw_1.GetSelectedRow(0) = 0 THEN
	pb_grabar.Enabled	=	False
ELSE
	pb_grabar.Enabled	=	True
END IF

end event

event dw_1::getfocus;//
end event

event dw_1::losefocus;//
end event

event dw_1::rowfocuschanged;//
end event

event type long dw_1::ue_seteafila(unsignedlong wparam, long lparam);RETURN 0  
end event

type st_encabe from w_mant_tabla`st_encabe within w_proc_rechazo_lote_sag
integer y = 16
integer width = 3031
integer height = 568
boolean enabled = true
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_proc_rechazo_lote_sag
integer x = 3255
integer y = 116
integer taborder = 0
boolean enabled = false
end type

event pb_lectura::clicked;IF cbx_1.Checked THEN
	istr_mant.argumento[1] 	= 	'-1'
ELSE
	istr_mant.argumento[1] 	= 	String(dw_cliente.Object.clie_codigo[1])
END IF	

istr_mant.argumento[2]	=	String(dw_planta.Object.plde_codigo[1])
istr_mant.argumento[3]	=	em_nrosag.Text

IF NOT existeinspeccion(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]), &
	                         Long(istr_mant.argumento[3])) THEN
	Parent.TriggerEvent("ue_recuperadatos")
ELSE									 
	em_nrosag.Text = ''
	em_nrosag.SetFocus()
END IF
end event

type pb_nuevo from w_mant_tabla`pb_nuevo within w_proc_rechazo_lote_sag
integer x = 3250
integer y = 416
integer taborder = 50
end type

event pb_nuevo::clicked;String ls_Null

SetNull (ls_Null)

pb_insertar.Enabled	= 	False
pb_imprimir.Enabled	= 	False
pb_eliminar.Enabled	= 	False
pb_grabar.Enabled		= 	False
pb_lectura.Enabled	=	False

em_nrosag.Text = ls_Null
em_nrosag.SetFocus()
dw_1.Reset()
dw_2.Reset()

//istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta
end event

type pb_insertar from w_mant_tabla`pb_insertar within w_proc_rechazo_lote_sag
boolean visible = false
integer x = 3250
integer y = 588
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_proc_rechazo_lote_sag
boolean visible = false
integer x = 3250
integer y = 768
integer taborder = 60
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_proc_rechazo_lote_sag
integer x = 3250
integer y = 948
integer taborder = 70
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_proc_rechazo_lote_sag
boolean visible = false
integer x = 3250
integer y = 1124
integer taborder = 80
end type

type pb_salir from w_mant_tabla`pb_salir within w_proc_rechazo_lote_sag
integer x = 3250
integer y = 1512
integer taborder = 90
end type

type st_3 from statictext within w_proc_rechazo_lote_sag
integer x = 453
integer y = 232
integer width = 448
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_cliente from datawindow within w_proc_rechazo_lote_sag
integer x = 1170
integer y = 220
integer width = 1152
integer height = 92
integer taborder = 10
boolean bringtotop = true
boolean enabled = false
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event dberror;RETURN 1
end event

event itemchanged;IF NoExisteCliente(Integer(data)) THEN
	This.SetItem(1, "clie_codigo", gi_codexport)
	RETURN 1
ELSE
	istr_mant.argumento[1]	=	data
	Idwc_planta.Retrieve(-1)
	istr_mant.argumento[2]	=	String(dw_planta.Object.plde_codigo[1])
	dw_planta.SetItem(1, "plde_codigo", Integer(istr_mant.argumento[2]))
END IF
end event

type st_4 from statictext within w_proc_rechazo_lote_sag
integer x = 453
integer y = 348
integer width = 453
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_proc_rechazo_lote_sag
integer x = 1170
integer y = 336
integer width = 969
integer height = 92
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event dberror;RETURN 1
end event

event itemchanged;IF NoExistePlanta(Integer(data)) THEN
	This.SetItem(1, "plde_codigo", gi_codplanta)
	
	RETURN 1
ELSE
	istr_mant.argumento[2]	=	String(data)
END IF
end event

type st_numero from statictext within w_proc_rechazo_lote_sag
integer x = 453
integer y = 468
integer width = 695
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Nro. Inspección S.A.G."
boolean focusrectangle = false
end type

type em_nrosag from editmask within w_proc_rechazo_lote_sag
integer x = 1170
integer y = 452
integer width = 462
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;
istr_mant.argumento[3]	=	This.Text

IF Long(This.Text) > 0 THEN
	pb_lectura.Enabled	=	True
END IF
end event

type em_fecha from editmask within w_proc_rechazo_lote_sag
integer x = 2272
integer y = 452
integer width = 485
integer height = 92
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_2 from statictext within w_proc_rechazo_lote_sag
integer x = 1705
integer y = 468
integer width = 549
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha Resolución "
alignment alignment = center!
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_2 from datawindow within w_proc_rechazo_lote_sag
boolean visible = false
integer x = 3643
integer y = 144
integer width = 274
integer height = 192
integer taborder = 100
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mues_rechazo_lote_sag_2"
richtexttoolbaractivation richtexttoolbaractivation = richtexttoolbaractivationalways!
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type rb_1 from radiobutton within w_proc_rechazo_lote_sag
integer x = 544
integer y = 92
integer width = 402
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
string text = "Aprobar"
boolean checked = true
end type

event clicked;//em_fecha.Visible = False
//st_2.Visible = False
end event

type rb_2 from radiobutton within w_proc_rechazo_lote_sag
integer x = 1161
integer y = 88
integer width = 402
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
string text = "Rechazar"
end type

event clicked;//em_fecha.Visible = True
//st_2.Visible = True
end event

type cbx_1 from checkbox within w_proc_rechazo_lote_sag
integer x = 2386
integer y = 232
integer width = 311
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
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	istr_mant.argumento[1] = '-1'
	dw_cliente.Enabled = False	
	dw_cliente.Reset()
	idwc_cliente.Retrieve(-1)
	dw_cliente.InsertRow(0)
ELSE
	dw_cliente.Enabled = True
//	idwc_cliente.Retrieve(-1)
//	dw_cliente.InsertRow(0)
//	dw_cliente.SetItem(1,"clie_codigo",	gi_CodExport)
//	istr_mant.argumento[1] = String(gi_CodExport)
END IF	
end event

type rb_3 from radiobutton within w_proc_rechazo_lote_sag
integer x = 1778
integer y = 88
integer width = 402
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
string text = "Objetar"
end type

event clicked;//em_fecha.Visible = True
//st_2.Visible = True
end event

type rb_borrar from radiobutton within w_proc_rechazo_lote_sag
event ue_validapassword ( )
integer x = 2331
integer y = 88
integer width = 402
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
string text = "Borrar"
end type

event ue_validapassword();Str_mant					lstr_mant

lstr_mant.Argumento[1]	=	"Produccion"
lstr_mant.Argumento[2]	=	gs_Password+''+'contraparte'

OpenWithParm(w_password, lstr_mant)

lstr_mant	=	Message.PowerObjectParm

IF lstr_mant.Respuesta = 0 THEN 
	MessageBox("Atención", "Ud NO Tiene Autorizacion Para Eliminar Inspecciones.~r~r", Exclamation!, Ok!)
	rb_1.Checked = True
END IF	
end event

event clicked;PostEvent("ue_validapassword")
end event

