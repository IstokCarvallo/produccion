$PBExportHeader$w_mant_mues_spro_detencionlineadeta.srw
forward
global type w_mant_mues_spro_detencionlineadeta from w_mant_tabla
end type
type gb_4 from groupbox within w_mant_mues_spro_detencionlineadeta
end type
type st_1 from statictext within w_mant_mues_spro_detencionlineadeta
end type
type dw_planta from datawindow within w_mant_mues_spro_detencionlineadeta
end type
type dw_linea from datawindow within w_mant_mues_spro_detencionlineadeta
end type
type st_2 from statictext within w_mant_mues_spro_detencionlineadeta
end type
type dw_especie from datawindow within w_mant_mues_spro_detencionlineadeta
end type
type st_3 from statictext within w_mant_mues_spro_detencionlineadeta
end type
type st_4 from statictext within w_mant_mues_spro_detencionlineadeta
end type
type st_5 from statictext within w_mant_mues_spro_detencionlineadeta
end type
type em_fecha from editmask within w_mant_mues_spro_detencionlineadeta
end type
type st_encabe3 from st_encabe within w_mant_mues_spro_detencionlineadeta
end type
type cb_buscaproceso from commandbutton within w_mant_mues_spro_detencionlineadeta
end type
type dw_area from datawindow within w_mant_mues_spro_detencionlineadeta
end type
type em_turno from editmask within w_mant_mues_spro_detencionlineadeta
end type
type st_turno from statictext within w_mant_mues_spro_detencionlineadeta
end type
type st_min from statictext within w_mant_mues_spro_detencionlineadeta
end type
type em_minutos from editmask within w_mant_mues_spro_detencionlineadeta
end type
end forward

global type w_mant_mues_spro_detencionlineadeta from w_mant_tabla
integer width = 3643
integer height = 2124
string title = "Detalle Detención Línea"
gb_4 gb_4
st_1 st_1
dw_planta dw_planta
dw_linea dw_linea
st_2 st_2
dw_especie dw_especie
st_3 st_3
st_4 st_4
st_5 st_5
em_fecha em_fecha
st_encabe3 st_encabe3
cb_buscaproceso cb_buscaproceso
dw_area dw_area
em_turno em_turno
st_turno st_turno
st_min st_min
em_minutos em_minutos
end type
global w_mant_mues_spro_detencionlineadeta w_mant_mues_spro_detencionlineadeta

type variables
w_mant_deta_spro_detencionlineadeta iw_mantencion

DataWindowChild idwc_areas,idwc_planta,idwc_linea,idwc_especie, idwc_detencion

uo_lineapacking iuo_lineapacking
uo_plantadesp 	 iuo_plantadesp
uo_especie		 iuo_especie

Boolean	ib_enlace
end variables

forward prototypes
public function boolean existeproceso ()
public function boolean existeencabezado (integer ai_tipo, string as_valor)
public function boolean validaminutos ()
public subroutine buscaproceso ()
protected function boolean wf_actualiza_db ()
end prototypes

public function boolean existeproceso ();Boolean	lb_Retorno
Integer	li_Planta, li_Linea, li_Especie, li_Cantidad, li_Turno
Date		ldt_Fecha, ldt_FechaNula
String   ls_fecha, ls_hora

li_Planta		= Integer(istr_Mant.Argumento[2])
li_Especie		= Integer(istr_Mant.Argumento[3])
li_Linea			= Integer(istr_Mant.Argumento[4])
ldt_fecha		= Date(istr_Mant.Argumento[5])
li_Turno			= Integer(istr_Mant.Argumento[6])

SELECT	Count(*)
	INTO	:li_Cantidad
	FROM	dba.spro_detencionlineaenca 
	WHERE	plde_codigo	=	:li_Planta
	AND	line_codigo	=	:li_linea
	AND	espe_codigo	=	:li_Especie
	AND	edla_fecpro	=	:ldt_Fecha 
	AND   rdla_turno	=	:li_Turno;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla Spro_detencionlineaenca ")
	
ELSEIF sqlca.SQLCode <> 100 and li_cantidad>0 THEN
	istr_mant.argumento[2]	=	String(li_Planta)
	istr_mant.argumento[3]	=	String(li_Linea)
	istr_mant.argumento[4]	=	String(li_Especie)
	istr_mant.argumento[5]	=	String(ldt_Fecha)
	istr_mant.argumento[6]	=	String(li_Turno)

	IF validaminutos() = False THEN
		This.TriggerEvent("ue_recuperadatos")
		lb_Retorno	=	False
	ELSE
		MessageBox("Atención","Area Seleccionada No Tiene Minutos A Justificar",Exclamation!)
		lb_Retorno	=	True
	END IF	
ELSE
	lb_Retorno	=	True
END IF
RETURN lb_Retorno
end function

public function boolean existeencabezado (integer ai_tipo, string as_valor);Boolean	lb_Retorno
Integer	li_Planta, li_Linea, li_Especie, li_estado, li_Turno,li_Area
Date	   ldt_Fecha, ldt_FechaNula
String   ls_fecha, ls_hora

li_area			=	dw_area.Object.area_codigo[1]
li_Planta		=	dw_planta.Object.plde_codigo[1]
li_Linea			=	dw_linea.Object.line_codigo[1]
li_Especie		=	dw_especie.Object.espe_codigo[1]
li_Turno			=	Integer(em_turno.text)
ldt_Fecha	   =	Date(em_fecha.text)	

CHOOSE CASE ai_tipo	
	CASE 1
		li_area		=  Integer(as_valor)
	CASE 3
		li_Especie	=	Integer(as_valor)
	CASE 4
		li_Linea		=	Integer(as_valor)
	CASE 5
		ldt_Fecha	=	Date(as_Valor)			
	CASE 6
		li_Turno		=	Integer(as_valor)
	CASE 7
		istr_mant.argumento[7] = "0"
		
END CHOOSE

SELECT	edla_estado
	INTO	:li_estado
	FROM	dba.spro_detencionlineaenca
	WHERE	plde_codigo	=	:li_Planta
	AND   line_codigo	=	:li_Linea
	AND	espe_codigo	=	:li_Especie
	AND	edla_fecpro	=	:ldt_Fecha 
	AND	rdla_turno	=	:li_Turno;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla Spro_detencionlineaenca ")
	RETURN TRUE
RETURN TRUE
ELSEIF sqlca.SQLCode <> 100 THEN
	istr_mant.argumento[2]	=	String(li_Planta)
	istr_mant.argumento[4]	=	String(li_Linea)
	istr_mant.argumento[3]	=	String(li_Especie)
	istr_mant.argumento[5]	=	String(ldt_Fecha)
	istr_mant.argumento[6]	=	String(li_Turno)
	istr_mant.argumento[7]	=  String(li_estado)
	

	IF validaminutos() = False AND ai_Tipo <> 7 THEN
		This.TriggerEvent("ue_recuperadatos")
		lb_Retorno	=	False
	ELSEIF ai_Tipo <> 7 THEN
		MessageBox("Atención","Area Seleccionada No Tiene Minutos A Justificar",Exclamation!)
		lb_Retorno	=	True
	END IF
ELSE
	lb_Retorno	=	True
END IF

RETURN lb_Retorno
end function

public function boolean validaminutos ();Boolean	lb_Retorno
Integer	li_Planta, li_Linea, li_Especie, li_Cantidad,li_Area,li_Minutos,li_fila, li_Turno
Date		ldt_Fecha, ldt_FechaNula
String   ls_fecha, ls_hora

li_Area			= Integer(istr_Mant.Argumento[1])
li_Planta		= gstr_ParamPlanta.CodigoPlanta
li_Especie		= Integer(istr_mant.argumento[3])
li_Linea			= Integer(istr_mant.argumento[4])
ldt_fecha		= Date(istr_mant.Argumento[5])
li_Turno			= Integer(istr_Mant.Argumento[6])

SELECT	rdla_minuto
	INTO	:li_Minutos
	FROM	dba.spro_detencionlinearesu
	WHERE	area_codigo =  :li_Area
	AND	plde_codigo	=	:li_Planta
	AND	line_codigo	=	:li_linea
	AND	espe_codigo	=	:li_Especie
	AND	edla_fecpro	=	:ldt_Fecha 
	AND   rdla_turno	=	:li_Turno;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla Spro_detencionlinearesu ")
	RETURN TRUE
ELSEIF sqlca.SQLCode = 0 THEN
		em_minutos.text = String(li_Minutos)
		RETURN FALSE
ELSE
		MessageBox("Atención","Area No Tiene Tienes Minutos a Justificar",Exclamation!)	
		RETURN TRUE
END IF	

end function

public subroutine buscaproceso ();Date  ldt_fecha
OpenWithParm(w_busc_detencionlinea, istr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.argum[2] <> "" THEN
	istr_mant.argumento[4] = istr_busq.argum[2]
	istr_mant.argumento[3] = istr_busq.argum[3]
	istr_mant.argumento[5] = istr_busq.argum[4]
	istr_Mant.Argumento[6] = istr_busq.argum[5]
	
	ldt_Fecha	=	Date(istr_mant.Argumento[5])
	dw_Linea.SetItem(1,"line_codigo",Integer(istr_mant.Argumento[4]))
	dw_Especie.SetItem(1,"espe_codigo",Integer(istr_mant.Argumento[3]))
	Em_Fecha.Text = String(ldt_fecha)
	Em_Turno.Text = istr_Mant.Argumento[6]
	
	
	IF validaminutos() = False THEN
		This.TriggerEvent("ue_recuperadatos")
	END IF	
END IF
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

event ue_nuevo();call super::ue_nuevo;istr_mant.borra	= False
istr_mant.agrega	= True

OpenWithParm(iw_mantencion, istr_mant)

istr_mant.argumento[1] = istr_mant.argumento[1] 
istr_mant.argumento[2] = istr_mant.argumento[2]

IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
	pb_eliminar.Enabled	= TRUE
	pb_grabar.Enabled		= TRUE
END IF

dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)
end event

event ue_imprimir();SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "DETENCION LINEAS"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_detencionlinea"

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(gstr_paramplanta.codigoPlanta,Integer(istr_mant.argumento[1]), &
								  Integer(istr_mant.argumento[4]),Integer(istr_mant.argumento[3]),&
								  Date(Mid(istr_mant.argumento[5],1,10)),Integer(istr_Mant.Argumento[6]))

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')

	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end event

event ue_recuperadatos();call super::ue_recuperadatos;Integer li_Minutos
Long	ll_fila, respuesta

li_Minutos = Integer(em_minutos.Text)
DO
	ll_fila	= dw_1.Retrieve(Integer(Istr_Mant.Argumento[1]),gstr_paramplanta.codigoPlanta,+ &
									Integer(istr_mant.argumento[3]),Integer(istr_mant.argumento[4]),+ &
									Date(Mid(istr_mant.Argumento[5], 1, 10)),Integer(istr_mant.argumento[6]), li_Minutos)
								
	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SelectRow(1,True)
		dw_1.SetFocus()
		pb_imprimir.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= False
	ELSE
		pb_insertar.SetFocus()
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN
	Close(This)
ELSE
	pb_insertar.Enabled	= True
END IF
end event

on w_mant_mues_spro_detencionlineadeta.create
int iCurrent
call super::create
this.gb_4=create gb_4
this.st_1=create st_1
this.dw_planta=create dw_planta
this.dw_linea=create dw_linea
this.st_2=create st_2
this.dw_especie=create dw_especie
this.st_3=create st_3
this.st_4=create st_4
this.st_5=create st_5
this.em_fecha=create em_fecha
this.st_encabe3=create st_encabe3
this.cb_buscaproceso=create cb_buscaproceso
this.dw_area=create dw_area
this.em_turno=create em_turno
this.st_turno=create st_turno
this.st_min=create st_min
this.em_minutos=create em_minutos
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.dw_planta
this.Control[iCurrent+4]=this.dw_linea
this.Control[iCurrent+5]=this.st_2
this.Control[iCurrent+6]=this.dw_especie
this.Control[iCurrent+7]=this.st_3
this.Control[iCurrent+8]=this.st_4
this.Control[iCurrent+9]=this.st_5
this.Control[iCurrent+10]=this.em_fecha
this.Control[iCurrent+11]=this.st_encabe3
this.Control[iCurrent+12]=this.cb_buscaproceso
this.Control[iCurrent+13]=this.dw_area
this.Control[iCurrent+14]=this.em_turno
this.Control[iCurrent+15]=this.st_turno
this.Control[iCurrent+16]=this.st_min
this.Control[iCurrent+17]=this.em_minutos
end on

on w_mant_mues_spro_detencionlineadeta.destroy
call super::destroy
destroy(this.gb_4)
destroy(this.st_1)
destroy(this.dw_planta)
destroy(this.dw_linea)
destroy(this.st_2)
destroy(this.dw_especie)
destroy(this.st_3)
destroy(this.st_4)
destroy(this.st_5)
destroy(this.em_fecha)
destroy(this.st_encabe3)
destroy(this.cb_buscaproceso)
destroy(this.dw_area)
destroy(this.em_turno)
destroy(this.st_turno)
destroy(this.st_min)
destroy(this.em_minutos)
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

event open;Date	ld_Fecha

x				= 0
y				= 0
This.Width	= dw_1.width + 540
This.Height	= 1993
This.Icon									=	Gstr_apl.Icono

IF IsValid(w_maed_detencionlinea) THEN
	IF ClassName(Message.PowerObjectParm) = 'str_busqueda' THEN
		istr_busq	=	Message.PowerObjectParm
		istr_Mant.Argumento[1]	=	istr_busq.Argum[1]
		istr_Mant.Argumento[2]	=	istr_busq.Argum[2]
		istr_Mant.Argumento[3]	=	istr_busq.Argum[3]
		istr_Mant.Argumento[4]	=	istr_busq.Argum[4]
		istr_Mant.Argumento[5]	=	istr_busq.Argum[5]
		istr_Mant.Argumento[6]	=	istr_busq.Argum[6]
				
		ib_enlace	=	True
		
		PostEvent("ue_recuperadatos")
	ELSE
		This.ParentWindow().ToolBarVisible	=	True
		im_menu	= m_principal

		im_menu.Item[1].Item[6].Enabled		=	True
		im_menu.Item[7].Visible					=	True
	END IF
ELSE
	This.ParentWindow().ToolBarVisible	=	True
	im_menu	= m_principal
	
	im_menu.Item[1].Item[6].Enabled		=	True
	im_menu.Item[7].Visible					=	True
END IF

dw_1.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.dw						=	dw_1
istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.Solo_Consulta			=	istr_mant.UsuarioSoloConsulta

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)

iuo_lineapacking = create uo_lineapacking
iuo_plantadesp	  = Create uo_plantadesp	
iuo_especie   	  = Create uo_especie	
////Area
dw_area.GetChild("area_codigo", idwc_areas)
idwc_areas.SetTransObject(SqlCa)
IF idwc_areas.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Area Responsables")
ELSE
	dw_area.SetTransObject(SqlCa)
	dw_area.InsertRow(0)
END IF

//Planta
dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(SqlCa)
IF idwc_planta.Retrieve(gi_codexport) = 0 THEN
	MessageBox("Atención","Falta Registrar Plantas")
ELSE
	dw_planta.SetTransObject(SqlCa)
	dw_planta.InsertRow(0)
END IF
dw_Planta.SetItem(1,"plde_codigo",gstr_paramplanta.codigoPlanta)
dw_Planta.Object.plde_codigo.BackGround.Color	=	RGB(192,192,192)

//linea
dw_linea.GetChild("line_codigo", idwc_linea)
idwc_linea.SetTransObject(SqlCa)
IF idwc_linea.Retrieve(0) = 0 THEN
	MessageBox("Atención","Falta Registrar Plantas")
ELSE
	dw_linea.SetTransObject(SqlCa)
	dw_linea.InsertRow(0)
END IF

//Especie
dw_Especie.GetChild("espe_codigo", idwc_Especie)
idwc_Especie.SetTransObject(SqlCa)
IF idwc_Especie.Retrieve(gstr_parempresa.empr_codexp) = 0 THEN
	MessageBox("Atención","Falta Registrar Plantas")
ELSE
	dw_Especie.SetTransObject(SqlCa)
	dw_Especie.InsertRow(0)
END IF

//Detención
dw_1.GetChild("deli_coddet", idwc_detencion)
idwc_detencion.SetTransObject(SqlCa)
IF idwc_detencion.Retrieve(0) = 0 THEN
	MessageBox("Atención","Falta Registrar Motivis Detención Líneas")
END IF

IF NOT ib_enlace THEN
	em_fecha.Text = String(Today())
	istr_Mant.Argumento[5] 	= String(Today())
	istr_Mant.Argumento[2]	= String(gstr_paramplanta.codigoPlanta)
ELSE
	ld_Fecha	=	Date(Mid(istr_mant.Argumento[5], 1, 10))
	dw_Linea.SetItem(1,"line_codigo",Integer(istr_mant.Argumento[4]))
	dw_Especie.SetItem(1,"espe_codigo",Integer(istr_mant.Argumento[3]))
	dw_area.SetItem(1,"area_codigo",Integer(istr_mant.Argumento[1]))
	Em_Fecha.Text = String(ld_fecha)
	Em_Turno.Text = istr_Mant.Argumento[6]
	validaminutos()
END IF

buscar	= "Código Area:Narea_codigo,Código Planta:plde_codigo,Fecha Proceso:Dedla_fecpro"
ordenar	= "Código Area:area_codigo,Código Planta:plde_codigo,Fecha Proceso:edla_fecpro"
end event

event ue_modifica;call super::ue_modifica;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False

	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

event close;IF ib_enlace THEN
	GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)
ELSE
	CALL SUPER::Close
END IF
end event

event ue_validaborrar();call super::ue_validaborrar;IF Not Existeencabezado(7,"0") THEN
	IF istr_mant.Argumento[7] = "2" THEN
		Messagebox("Error","Registro se encuentra cerrado, no es posible eliminar")
		Message.DoubleParm = -1
	ELSE
		IF MessageBox("Borrar Registro","Desea Borrar la Información ?", Question!, YesNo!) = 1 THEN
			Message.DoubleParm = 1
		ELSE
			Message.DoubleParm = -1
		END IF
	END IF
END IF

RETURN
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_spro_detencionlineadeta
integer x = 73
integer y = 528
integer width = 3054
integer height = 1380
integer taborder = 120
string dataobject = "dw_mues_spro_detencionlineadeta"
boolean hscrollbar = true
end type

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_spro_detencionlineadeta
boolean visible = false
integer x = 64
integer width = 3072
integer height = 440
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_spro_detencionlineadeta
boolean visible = false
integer x = 3305
integer y = 168
integer taborder = 60
boolean enabled = false
end type

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_spro_detencionlineadeta
integer x = 3305
integer y = 464
integer taborder = 0
end type

event pb_nuevo::clicked;Integer li_null
SetNull(li_Null)
pb_insertar.Enabled		= False
pb_eliminar.Enabled		= False
pb_grabar.Enabled			= False
pb_imprimir.Enabled		= False
pb_Lectura.Enabled		= False

cb_buscaProceso.Enabled = False
dw_Area.SetItem(1,"area_codigo",Integer(li_null))
dw_Linea.SetItem(1,"line_codigo",Integer(li_null))
dw_Especie.SetItem(1,"espe_codigo",Integer(li_null))
dw_Area.SetFocus()
dw_1.Reset()
EM_turno.Text				= ''
em_minutos.Text			= ''
istr_Mant.Argumento[1]	= ""
istr_Mant.Argumento[3]	= ""
istr_Mant.Argumento[4]  = ""
istr_Mant.Argumento[5] 	= String(Today())
RETURN 0

end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_spro_detencionlineadeta
integer x = 3305
integer y = 640
integer taborder = 70
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_spro_detencionlineadeta
integer x = 3305
integer y = 816
integer taborder = 80
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_spro_detencionlineadeta
integer x = 3305
integer y = 992
integer taborder = 90
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_spro_detencionlineadeta
integer x = 3305
integer y = 1172
integer taborder = 100
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_spro_detencionlineadeta
integer x = 3314
integer y = 1556
integer height = 140
integer taborder = 110
end type

type gb_4 from groupbox within w_mant_mues_spro_detencionlineadeta
integer x = 128
integer y = 80
integer width = 2953
integer height = 348
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type st_1 from statictext within w_mant_mues_spro_detencionlineadeta
integer x = 1335
integer y = 164
integer width = 165
integer height = 84
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Area"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_mant_mues_spro_detencionlineadeta
integer x = 389
integer y = 156
integer width = 869
integer height = 100
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_planta"
boolean border = false
boolean livescroll = true
end type

event itemerror;Return 1
end event

type dw_linea from datawindow within w_mant_mues_spro_detencionlineadeta
integer x = 1495
integer y = 292
integer width = 549
integer height = 100
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_lineapacking"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer li_null
SetNull(li_null)
IF iuo_lineapacking.Existe(gstr_paramplanta.codigoPlanta,Integer(data),True,Sqlca) = True THEN
	istr_Mant.Argumento[4] = Data
	IF ExisteEncabezado(4,data) = FALSE THEN
		pb_lectura.Enabled = True
	END IF
	return 0
ELSE	
	dw_linea.SetItem(1,"line_codigo",li_null)
	RETURN 1
END IF	

end event

event itemerror;Return 1
end event

type st_2 from statictext within w_mant_mues_spro_detencionlineadeta
integer x = 1335
integer y = 300
integer width = 155
integer height = 84
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Linea"
boolean focusrectangle = false
end type

type dw_especie from datawindow within w_mant_mues_spro_detencionlineadeta
integer x = 389
integer y = 288
integer width = 869
integer height = 92
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_especies"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer li_Null
SetNull(li_Null)
IF iuo_especie.Existe(Integer(data),True,Sqlca)  = True THEN
	istr_Mant.Argumento[3] = Data
	IF ExisteEncabezado(3,data) = FALSE THEN
		pb_lectura.Enabled = True
	END IF	
ELSE
	dw_Especie.SetItem(1,"espe_codigo",li_Null)
	RETURN 1
END IF	
end event

event itemerror;return 1
end event

type st_3 from statictext within w_mant_mues_spro_detencionlineadeta
integer x = 169
integer y = 296
integer width = 206
integer height = 84
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Especie"
boolean focusrectangle = false
end type

type st_4 from statictext within w_mant_mues_spro_detencionlineadeta
integer x = 2066
integer y = 308
integer width = 169
integer height = 84
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Fecha"
boolean focusrectangle = false
end type

type st_5 from statictext within w_mant_mues_spro_detencionlineadeta
integer x = 169
integer y = 164
integer width = 178
integer height = 84
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type em_fecha from editmask within w_mant_mues_spro_detencionlineadeta
integer x = 2249
integer y = 292
integer width = 334
integer height = 100
integer taborder = 40
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_Mant.Argumento[5] = This.text
IF ExisteEncabezado(5,This.text) = FALSE THEN
	pb_lectura.Enabled = True
END IF	

end event

type st_encabe3 from st_encabe within w_mant_mues_spro_detencionlineadeta
boolean visible = true
integer x = 82
integer y = 72
integer width = 3054
integer height = 400
integer textsize = -9
fontcharset fontcharset = ansi!
end type

type cb_buscaproceso from commandbutton within w_mant_mues_spro_detencionlineadeta
integer x = 2459
integer y = 156
integer width = 110
integer height = 100
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "..."
end type

event clicked;BuscaProceso()
end event

type dw_area from datawindow within w_mant_mues_spro_detencionlineadeta
integer x = 1495
integer y = 156
integer width = 878
integer height = 100
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_area"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_Mant.Argumento[1] 	= Data
cb_buscaProceso.Enabled = True

dw_1.GetChild("deli_coddet", idwc_detencion)
idwc_detencion.SetTransObject(SqlCa)
IF idwc_detencion.Retrieve(Integer(Data)) = 0 THEN
	MessageBox("Atención","Falta Registrar Motivos Detención Líneas")
END IF

IF ExisteEncabezado(1,data) = FALSE THEN
	pb_lectura.Enabled = True
END IF	

end event

event itemerror;Return 1
end event

type em_turno from editmask within w_mant_mues_spro_detencionlineadeta
integer x = 2871
integer y = 292
integer width = 142
integer height = 100
integer taborder = 50
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "#"
string minmax = "~~1"
end type

event modified;istr_Mant.Argumento[6]	= This.Text

IF ExisteEncabezado(6,This.text) = FALSE THEN
	pb_lectura.Enabled = True
END IF	
end event

type st_turno from statictext within w_mant_mues_spro_detencionlineadeta
integer x = 2629
integer y = 296
integer width = 229
integer height = 84
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Turno"
boolean focusrectangle = false
end type

type st_min from statictext within w_mant_mues_spro_detencionlineadeta
integer x = 2629
integer y = 164
integer width = 224
integer height = 84
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Tot. Min."
boolean focusrectangle = false
end type

type em_minutos from editmask within w_mant_mues_spro_detencionlineadeta
integer x = 2871
integer y = 156
integer width = 142
integer height = 100
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "#"
string minmax = "~~1"
end type

event modified;istr_Mant.Argumento[6]	= This.Text
ExisteProceso()
pb_lectura.Enabled = True
end event

