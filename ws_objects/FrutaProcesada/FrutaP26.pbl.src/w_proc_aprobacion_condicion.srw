$PBExportHeader$w_proc_aprobacion_condicion.srw
$PBExportComments$Genera Archivo Plano SAG de Eliminación de Pallets.
forward
global type w_proc_aprobacion_condicion from w_mant_tabla
end type
type st_3 from statictext within w_proc_aprobacion_condicion
end type
type dw_cliente from datawindow within w_proc_aprobacion_condicion
end type
type st_4 from statictext within w_proc_aprobacion_condicion
end type
type dw_planta from datawindow within w_proc_aprobacion_condicion
end type
type st_numero from statictext within w_proc_aprobacion_condicion
end type
type em_nrosag from editmask within w_proc_aprobacion_condicion
end type
type em_fecha from editmask within w_proc_aprobacion_condicion
end type
type st_2 from statictext within w_proc_aprobacion_condicion
end type
type dw_2 from datawindow within w_proc_aprobacion_condicion
end type
type rb_1 from radiobutton within w_proc_aprobacion_condicion
end type
type rb_2 from radiobutton within w_proc_aprobacion_condicion
end type
type cbx_1 from checkbox within w_proc_aprobacion_condicion
end type
type rb_3 from radiobutton within w_proc_aprobacion_condicion
end type
type rb_borrar from radiobutton within w_proc_aprobacion_condicion
end type
type dw_3 from datawindow within w_proc_aprobacion_condicion
end type
type st_5 from statictext within w_proc_aprobacion_condicion
end type
type em_rechazo from editmask within w_proc_aprobacion_condicion
end type
type st_6 from statictext within w_proc_aprobacion_condicion
end type
end forward

global type w_proc_aprobacion_condicion from w_mant_tabla
integer width = 3566
integer height = 1908
string title = "APROBACIÓN O RECHAZO LOTES SAG"
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
dw_3 dw_3
st_5 st_5
em_rechazo em_rechazo
st_6 st_6
end type
global w_proc_aprobacion_condicion w_proc_aprobacion_condicion

type variables
Integer			ii_PlantaSag
Date				id_fecha

w_gene_deta_archivo_eliminacion	iw_mantencion

DataWindowChild	idwc_cliente, idwc_planta, idwc_condicion

uo_condicion    iuo_condicion  
end variables

forward prototypes
public function integer codigoplantasag (integer planta)
public function boolean noexistecliente (integer ai_cliente)
public function boolean noexisteplanta (integer ai_planta)
protected function boolean wf_actualiza_db ()
public subroutine borra_inspecciones (integer ai_planta, integer ai_cliente, integer al_numero)
public function boolean existefumigacion (integer ai_cliente, integer ai_planta, long al_numero, integer ai_condicion)
end prototypes

event ue_validapassword();Str_mant					lstr_mant

lstr_mant.Argumento[1]	=	"Produccion"
lstr_mant.Argumento[2]	=	gs_Password+''+'contraparte'

OpenWithParm(w_password, lstr_mant)

lstr_mant	=	Message.PowerObjectParm

//IF istr_mant.Respuesta = 0 THEN Close(This)
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
Long		ll_numero
Integer	li_cliente,li_planta,li_condicion
String	ls_causalrec

//If Not dw_2.uf_check_required(0) Then RETURN False

li_cliente 	= dw_1.Object.clie_codigo[1]
li_planta		= dw_1.Object.plde_codigo[1]
ll_numero	= Long(em_nrosag.Text)
li_condicion	= dw_3.Object.cond_codigo[1]
ls_causalrec	= em_rechazo.Text

If Not dw_1.uf_validate(0) Then RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

	If dw_2.Update(True, False) = 1 Then
		If dw_1.Update(True, False) = 1 Then
			Commit;
			
			If sqlca.SQLCode <> 0 Then
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			Else
				lb_Retorno	=	True
				
				dw_1.ResetUpdate()
				dw_2.ResetUpdate()
				
				If rb_1.Checked Then
					//aprobado
					UPDATE dbo.fumigaenc set
						fumi_estado = 1
						WHERE clie_codigo = :li_cliente
						AND plde_codigo = :li_planta
						AND fumi_numsag = :ll_numero
						AND cond_codigo = :li_condicion;
						
				ElseIf rb_2.Checked Then
					//rechazado
					UPDATE dbo.fumigaenc set
						fumi_estado = 2,
						fumi_rechaz = :ls_causalrec
						WHERE clie_codigo = :li_cliente
						AND plde_codigo = :li_planta
						AND fumi_numsag = :ll_numero
						AND cond_codigo = :li_condicion;
						
				ElseIf rb_borrar.Checked Then
					
					DELETE dbo.fumigadet
					FROM dbo.fumigaenc
					WHERE dbo.fumigadet.clie_codigo = :li_cliente
						AND dbo.fumigadet.plde_codigo = :li_planta
						AND dbo.fumigaenc.fumi_numsag = :ll_numero
						AND dbo.fumigadet.cond_codigo = :li_condicion
						AND dbo.fumigadet.clie_codigo = dbo.fumigaenc.clie_codigo
						AND dbo.fumigadet.plde_codigo = dbo.fumigaenc.plde_codigo
						AND dbo.fumigadet.fumi_numero = dbo.fumigaenc.fumi_numero
						AND dbo.fumigadet.cond_codigo = dbo.fumigaenc.cond_codigo;
					COMMIT;
					
					DELETE dbo.fumigaenc
					WHERE clie_codigo = :li_cliente
						AND plde_codigo = :li_planta
						AND fumi_numsag = :ll_numero
						AND cond_codigo = :li_condicion;
						COMMIT;
									
				End If
			End If
		Else
			F_ErrorBaseDatos(sqlca, This.Title)
			RollBack;
		End If
	Else
		F_ErrorBaseDatos(sqlca, This.Title)
	End If

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
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

public function boolean existefumigacion (integer ai_cliente, integer ai_planta, long al_numero, integer ai_condicion);Date	ld_fecha
Long	ll_cuenta

SELECT	count(*) 
	INTO	:ll_cuenta
	FROM	dbo.fumigaenc
	WHERE	:ai_Cliente in (-1,clie_codigo)
	AND	plde_codigo = :ai_planta
	AND	fumi_numsag = :al_numero
	AND	fumi_estado = 7
	AND	cond_codigo = :ai_condicion;
			
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla fumigaenc")
	
	RETURN True
ELSEIF ll_cuenta = 0 THEN
	MessageBox("Atención", "Proceso de CONDICION No Ha Sido Definido O Ya Se Encuentra en Estado~r Definitivo .~r~r" + &
					"Ingrese o Seleccione Otro Número.", Exclamation!, Ok!)
					
	RETURN True
ELSE
	SELECT	max(fumi_fecfum)
	INTO	:ld_fecha
	FROM	dbo.fumigaenc
	WHERE	:ai_Cliente in (-1,clie_codigo)
	AND	plde_codigo = :ai_planta
	AND	fumi_numsag = :al_numero
	AND	fumi_estado = 7
	AND	cond_codigo = :ai_condicion;
	
	em_fecha.Text = String(ld_fecha)
	
	RETURN False
END IF
end function

on w_proc_aprobacion_condicion.create
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
this.dw_3=create dw_3
this.st_5=create st_5
this.em_rechazo=create em_rechazo
this.st_6=create st_6
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
this.Control[iCurrent+15]=this.dw_3
this.Control[iCurrent+16]=this.st_5
this.Control[iCurrent+17]=this.em_rechazo
this.Control[iCurrent+18]=this.st_6
end on

on w_proc_aprobacion_condicion.destroy
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
destroy(this.dw_3)
destroy(this.st_5)
destroy(this.em_rechazo)
destroy(this.st_6)
end on

event open;call super::open;x = 0
y = 0

This.Icon	=	Gstr_apl.Icono

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_cliente.InsertRow(0)
//dw_cliente.SetItem(1,"clie_codigo",0)

dw_3.GetChild("cond_codigo", idwc_condicion)
idwc_condicion.SetTransObject(sqlca)
idwc_condicion.Retrieve()
dw_3.InsertRow(0)
dw_3.SetItem(1, "cond_codigo", 1)

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

event ue_nuevo();//istr_mant.borra			= False
//istr_mant.agrega			= True
//
//OpenWithParm(iw_mantencion, istr_mant)
//
//IF dw_1.RowCount() > 0 AND pb_eliminar.Enabled = FALSE THEN
//	pb_eliminar.Enabled	= TRUE
//	pb_grabar.Enabled		= TRUE
//END IF
//
//dw_1.SetRow(il_fila)
//dw_1.SelectRow(il_fila,True)
//
//
end event

event ue_modifica();//IF dw_1.RowCount() > 0 THEN
//	istr_mant.agrega	= False
//	istr_mant.borra	= False
//
//	OpenWithParm(iw_mantencion, istr_mant)
//END IF
end event

event closequery;//
end event

event ue_recuperadatos;Long	ll_fila, respuesta,ll_fila1

dw_1.Reset()
dw_2.Reset()

DO
	ll_fila	= dw_1.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]), &
	                         Long(istr_mant.argumento[3]),dw_3.Object.cond_codigo[1])
	
	ll_fila	= dw_2.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]), &
	                         Long(istr_mant.argumento[3]),dw_3.Object.cond_codigo[1])
	
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

event ue_borrar();//IF dw_1.rowcount() < 1 THEN RETURN
//
//SetPointer(HourGlass!)
//
//ib_borrar = True
//w_main.SetMicroHelp("Validando la eliminación...")
//
//Message.DoubleParm = 0
//
//This.TriggerEvent ("ue_validaborrar")
//
//IF Message.DoubleParm = -1 THEN RETURN
//
//istr_mant.borra		= True
//istr_mant.agrega	= False
//
//OpenWithParm(iw_mantencion, istr_mant)
//
//istr_mant = Message.PowerObjectParm
//
//IF istr_mant.respuesta = 1 THEN
//	IF dw_1.DeleteRow(0) = 1 THEN
//		ib_borrar = False
//		w_main.SetMicroHelp("Borrando Registro...")
//		SetPointer(Arrow!)
//	ELSE
//		ib_borrar = False
//		MessageBox(This.Title,"No se puede borrar actual registro.")
//	END IF
//
// IF dw_1.RowCount() = 0 THEN
//		pb_eliminar.Enabled = False
//	ELSE
//		il_fila = dw_1.GetRow()
//		dw_1.SelectRow(il_fila,True)
//	END IF
//END IF
//
//istr_mant.borra	 = False
end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_Cliente, li_Planta, li_tipoin
Long		ll_Fila, ll_Numero, ll_numinp, ll_null,ll_Fila1
Date		ld_fechai
String		ls_fecha

SetNull(ll_null)

ls_fecha = em_fecha.text

FOR ll_Fila = 1 TO dw_1.RowCount()
	If rb_1.Checked Then	//aprobado
		dw_1.Object.cond_codigo[ll_Fila]	=	dw_1.Object.cond_fumigacion[ll_Fila]	
	ElseIf rb_2.Checked Then	//rechazado
		If dw_3.Object.cond_codigo[1] = 2 Then
			dw_1.Object.cond_codigo[ll_Fila]	=	4
		Else
			dw_1.Object.cond_codigo[ll_Fila]	=	0
		End If
	ElseIf rb_borrar.Checked Then
		dw_1.Object.cond_codigo[ll_Fila] = 0
		dw_1.Object.fumi_numero[ll_Fila] = ll_null
	End If
	dw_1.Object.paen_fecter[ll_Fila] = DateTime(Today(), Now())
NEXT

FOR ll_Fila1 = 1 TO dw_2.RowCount()
	dw_2.Object.cond_codigo[ll_Fila1] = 0
	If rb_1.Checked Then		//aprobado
		dw_2.Object.cond_codigo[ll_Fila1]	=	dw_2.Object.cond_fumigacion[ll_Fila1]
	ElseIf rb_2.Checked Then		//rechazado
		If dw_3.Object.cond_codigo[1]	= 2 Then
			dw_2.Object.cond_codigo[ll_Fila1]	=	4
		Else
			dw_2.Object.cond_codigo[ll_Fila1]	=	0
		End If
	ElseIf rb_borrar.Checked Then
		dw_2.Object.cond_codigo[ll_Fila1] 	= 0
	End If
NEXT
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
	li_estado = 2
END IF

IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)
w_main.SetMicroHelp("Grabando información...")
Message.DoubleParm = 0
TriggerEvent("ue_antesguardar")

IF rb_2.Checked THEN
	ld_fecha = Date(em_fecha.text)
ELSE
	ld_fecha = Date('19000101')
END IF	

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
	dw_1.Reset()
	dw_2.Reset()
	MessageBox("Atención", "Datos Grabados Exitosamente", Exclamation!, Ok!)
	pb_grabar.Enabled = False
	em_nrosag.Text = ''
	
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

type dw_1 from w_mant_tabla`dw_1 within w_proc_aprobacion_condicion
integer x = 69
integer y = 672
integer width = 2976
integer height = 1016
integer taborder = 0
string dataobject = "dw_mues_aprobacion_condicion"
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

event dw_1::ue_seteafila;RETURN 0  
end event

type st_encabe from w_mant_tabla`st_encabe within w_proc_aprobacion_condicion
integer x = 69
integer y = 28
integer width = 2976
integer height = 632
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_proc_aprobacion_condicion
integer x = 3173
integer y = 128
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

IF NOT existefumigacion(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]), &
	                         Long(istr_mant.argumento[3]),dw_3.Object.cond_codigo[1]) THEN
	Parent.TriggerEvent("ue_recuperadatos")
ELSE									 
	em_nrosag.Text = ''
	em_nrosag.SetFocus()
END IF
end event

type pb_nuevo from w_mant_tabla`pb_nuevo within w_proc_aprobacion_condicion
integer x = 3173
integer y = 428
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

type pb_insertar from w_mant_tabla`pb_insertar within w_proc_aprobacion_condicion
boolean visible = false
integer x = 3173
integer y = 600
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_proc_aprobacion_condicion
boolean visible = false
integer x = 3173
integer y = 780
integer taborder = 60
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_proc_aprobacion_condicion
integer x = 3173
integer y = 960
integer taborder = 70
end type

event pb_grabar::clicked;IF rb_2.Checked THEN
	IF em_rechazo.Text = '' THEN
		MessageBox("Atención", "Falta Causal de Rechazo para Continuar.", Exclamation!, Ok!)
		em_rechazo.SetFocus()
		Return
	END IF
END IF

Parent.TriggerEvent("ue_guardar")

Close(Parent)
end event

type pb_imprimir from w_mant_tabla`pb_imprimir within w_proc_aprobacion_condicion
boolean visible = false
integer x = 3168
integer y = 1136
integer taborder = 80
end type

type pb_salir from w_mant_tabla`pb_salir within w_proc_aprobacion_condicion
integer x = 3173
integer y = 1524
integer taborder = 90
end type

type st_3 from statictext within w_proc_aprobacion_condicion
integer x = 133
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

type dw_cliente from datawindow within w_proc_aprobacion_condicion
integer x = 727
integer y = 216
integer width = 1170
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
	istr_mant.argumento[1]	=	String(Integer(data), '000')
	idwc_planta.Retrieve(1)
	istr_mant.argumento[2]	=	String(dw_planta.Object.plde_codigo[1])
	dw_planta.SetItem(1, "plde_codigo", Integer(istr_mant.argumento[2]))
END IF
end event

type st_4 from statictext within w_proc_aprobacion_condicion
integer x = 133
integer y = 324
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

type dw_planta from datawindow within w_proc_aprobacion_condicion
integer x = 727
integer y = 312
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

type st_numero from statictext within w_proc_aprobacion_condicion
integer x = 133
integer y = 532
integer width = 590
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
string text = "Nro. Condición SAG"
boolean focusrectangle = false
end type

type em_nrosag from editmask within w_proc_aprobacion_condicion
integer x = 736
integer y = 512
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

event modified;IF NOT isnull(dw_3.Object.cond_codigo[1]) THEN
	istr_mant.argumento[3]	=	This.Text
	
	IF Long(This.Text) > 0 THEN
		pb_lectura.Enabled	=	True
	END IF
ELSE
	MessageBox("Atención", "Falta el ingreso de tipo condición.~r~r" + &
					"Ingrese o seleccione tipo condición.", Exclamation!, Ok!)
	This.Text = ''					
END IF	
end event

type em_fecha from editmask within w_proc_aprobacion_condicion
integer x = 2240
integer y = 412
integer width = 507
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
boolean dropdowncalendar = true
end type

type st_2 from statictext within w_proc_aprobacion_condicion
integer x = 1687
integer y = 428
integer width = 553
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
string text = "Fecha Condición"
alignment alignment = center!
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_2 from datawindow within w_proc_aprobacion_condicion
boolean visible = false
integer x = 3077
integer y = 20
integer width = 146
integer height = 136
integer taborder = 100
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_aprueba_fumigacion"
boolean border = false
borderstyle borderstyle = stylelowered!
end type

type rb_1 from radiobutton within w_proc_aprobacion_condicion
integer x = 759
integer y = 80
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

IF This.Checked THEN
	em_rechazo.Enabled  = False
	em_rechazo.Text = ''
ELSE	
	em_rechazo.Enabled  = True
	em_rechazo.Text = ''
END IF	
end event

type rb_2 from radiobutton within w_proc_aprobacion_condicion
integer x = 1376
integer y = 76
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
IF This.Checked THEN
	em_rechazo.Enabled  = True
	em_rechazo.Text = ''
ELSE	
	em_rechazo.Enabled  = False
	em_rechazo.Text = ''
END IF	
end event

type cbx_1 from checkbox within w_proc_aprobacion_condicion
integer x = 1925
integer y = 228
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
	idwc_cliente.Retrieve()
	dw_cliente.InsertRow(0)
ELSE
	dw_cliente.Enabled = True
	idwc_cliente.Retrieve()
//	dw_cliente.InsertRow(0)
//	dw_cliente.SetItem(1,"clie_codigo",	gi_CodExport)
//	istr_mant.argumento[1] = String(gi_CodExport)
END IF	
end event

type rb_3 from radiobutton within w_proc_aprobacion_condicion
boolean visible = false
integer x = 2738
integer y = 1688
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
boolean enabled = false
string text = "Objetar"
end type

event clicked;//em_fecha.Visible = True
//st_2.Visible = True
end event

type rb_borrar from radiobutton within w_proc_aprobacion_condicion
event ue_validapassword ( )
integer x = 1993
integer y = 80
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

type dw_3 from datawindow within w_proc_aprobacion_condicion
integer x = 727
integer y = 408
integer width = 1006
integer height = 96
integer taborder = 50
boolean bringtotop = true
string dataobject = "dddw_condicion"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer li_Null

SetNull(li_Null)
iuo_condicion  = Create uo_condicion

IF iuo_condicion.Existe(Integer(Data),True,SqlCa) THEN
   istr_mant.argumento[9] = Data
	
ELSE
	This.SetItem(1,"cond_codigo",li_Null)
	
	RETURN 1
END IF


end event

event itemerror;RETURN 1
end event

type st_5 from statictext within w_proc_aprobacion_condicion
integer x = 133
integer y = 424
integer width = 297
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
string text = "Condición"
boolean focusrectangle = false
end type

type em_rechazo from editmask within w_proc_aprobacion_condicion
integer x = 1723
integer y = 512
integer width = 1280
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
boolean enabled = false
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
boolean autoskip = true
string minmax = "~~100"
end type

event modified;IF NOT isnull(dw_3.Object.cond_codigo[1]) THEN
	istr_mant.argumento[3]	=	This.Text
	
	IF Long(This.Text) > 0 THEN
		pb_lectura.Enabled	=	True
	END IF
ELSE
	MessageBox("Atención", "Falta el ingreso de tipo condición.~r~r" + &
					"Ingrese o seleccione tipo condición.", Exclamation!, Ok!)
	This.Text = ''					
END IF	
end event

type st_6 from statictext within w_proc_aprobacion_condicion
integer x = 1230
integer y = 532
integer width = 480
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
string text = "Causal Rechazo"
boolean focusrectangle = false
end type

