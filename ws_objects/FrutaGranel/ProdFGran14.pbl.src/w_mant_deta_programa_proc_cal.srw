$PBExportHeader$w_mant_deta_programa_proc_cal.srw
$PBExportComments$Mantención Detalle de Lotes en Recepción de Huerto.
forward
global type w_mant_deta_programa_proc_cal from w_mant_detalle_csd
end type
type dw_2 from uo_dw within w_mant_deta_programa_proc_cal
end type
type pb_ins_det from picturebutton within w_mant_deta_programa_proc_cal
end type
type pb_eli_det from picturebutton within w_mant_deta_programa_proc_cal
end type
type rb_directo from radiobutton within w_mant_deta_programa_proc_cal
end type
type rb_porce from radiobutton within w_mant_deta_programa_proc_cal
end type
type st_1 from statictext within w_mant_deta_programa_proc_cal
end type
type em_total from editmask within w_mant_deta_programa_proc_cal
end type
type gb_4 from groupbox within w_mant_deta_programa_proc_cal
end type
end forward

global type w_mant_deta_programa_proc_cal from w_mant_detalle_csd
integer width = 3237
integer height = 1976
event ue_nuevo_detalle ( )
event ue_borra_detalle ( )
dw_2 dw_2
pb_ins_det pb_ins_det
pb_eli_det pb_eli_det
rb_directo rb_directo
rb_porce rb_porce
st_1 st_1
em_total em_total
gb_4 gb_4
end type
global w_mant_deta_programa_proc_cal w_mant_deta_programa_proc_cal

type variables
DataWindowChild	idwc_calibre, idwc_especie, idwc_recibidor,idwc_embalaje, idwc_etiqueta, idwc_envase
uo_ProdCuarteles		iuo_Cuartel
uo_Productores		iuo_Productor

Integer	il_Fila_det
Long     	il_total
end variables

forward prototypes
public function boolean noexistecalibre (string as_valor)
public function boolean existecalibres ()
public subroutine redistribuyecalibres ()
public subroutine captura_totales ()
public function boolean duplicado (string as_columna, string as_valor)
end prototypes

event ue_nuevo_detalle();String   ls_Fecha
Date ldt_Fecha

dw_2.SetColumn("pprc_calini")

ls_Fecha  = Mid(istr_Mant.Argumento[3],1,10)
ldt_Fecha = Date(ls_Fecha)

il_Fila_det = dw_2.InsertRow(0)

dw_2.ScrollToRow(il_Fila_det)
dw_2.SetRow(il_Fila_det)
dw_2.SetFocus()

dw_2.Object.plde_codigo[il_Fila_det]	=	Integer(istr_Mant.Argumento[1])
dw_2.Object.espe_codigo[il_Fila_det]	=	Integer(istr_Mant.Argumento[2])
dw_2.Object.ppre_numero[il_Fila_det]	=	Long(istr_Mant.Argumento[3])
dw_2.Object.pprd_secuen[il_Fila_det]	=	dw_1.Object.pprd_secuen[il_Fila]
//dw_2.Object.clie_codigo[il_Fila_det]	=	Integer(istr_Mant.Argumento[13])

IF rb_porce.Checked THEN
	dw_2.Object.pprc_cancaj.Protect = 1
	dw_2.Object.pprc_pordis.Protect = 0
ELSE
	dw_2.Object.pprc_cancaj.Protect = 0
	dw_2.Object.pprc_pordis.Protect = 1
END IF
end event

event ue_borra_detalle();IF dw_2.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
	IF dw_2.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

	IF dw_2.RowCount() = 0 THEN
		pb_eli_det.Enabled = False
	ELSE
		il_fila = dw_2.GetRow()
		il_fila_det = dw_2.GetRow()
	END IF
	Captura_Totales()
END IF
end event

public function boolean noexistecalibre (string as_valor);Integer	li_especie,li_tipo,li_envase
String	ls_calibre,ls_nombre

li_especie	=	Integer(istr_Mant.Argumento[2])
li_tipo	 	=	Integer(istr_Mant.Argumento[7])
li_envase	=	Integer(istr_Mant.Argumento[8])
ls_calibre	=	as_valor

IF IsNull(ls_calibre) = False OR ls_calibre <>'' THEN
  SELECT caen_calibr  
    INTO :ls_nombre  
    FROM dbo.calibresenvase  
    WHERE ( espe_codigo = :li_especie ) AND  
          ( enva_tipoen = :li_tipo ) AND  
          ( enva_codigo = :li_envase ) AND  
          ( caen_calibr = :ls_calibre ) ;

	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Calibresenvases")
		RETURN True
	ELSEIF sqlca.SQLCode = 100 THEN
		
		MessageBox("Atención", " Código de Calibre no ha sido creado, Ingrese otro Código.", &
						Exclamation!, OK!)						
		RETURN True
	ELSE
		RETURN False
	END IF
END IF

RETURN True
end function

public function boolean existecalibres ();Integer	li_especie,li_tipo,li_envase,li_cont

li_especie	=	Integer(istr_Mant.Argumento[2])
li_tipo		=	Integer(istr_Mant.Argumento[7])
li_envase	=	Integer(istr_Mant.Argumento[8])

SELECT count(caen_calibr)  
  INTO :li_cont  
  FROM dbo.calibresenvase  
  WHERE ( espe_codigo = :li_especie ) AND  
        ( enva_tipoen = :li_tipo ) AND  
        ( enva_codigo = :li_envase ) ;
			
IF sqlca.SQLCode = -1 THEN
F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Calibresenvases")
	RETURN FALSE
ELSEIF li_cont > 0 THEN
	RETURN TRUE
ELSE
	RETURN False
END IF		
end function

public subroutine redistribuyecalibres ();Long	ll_Fila, ll_Filas, ll_Cajas

ll_Filas	=	dw_2.RowCount()

FOR ll_Fila = 1 TO ll_Filas
	ll_Cajas	=	Round(il_total * dw_2.Object.pprc_pordis[ll_Fila]/100,0)
	dw_2.Object.pprc_cancaj[ll_Fila]	=	ll_Cajas
NEXT

Captura_Totales()
end subroutine

public subroutine captura_totales ();Long	ll_Fila,ll_Total_Cajas

ll_Fila	=	dw_2.RowCount()

dw_2.AcceptText()

IF ll_Fila > 0 THEN
	ll_Total_Cajas		=	dw_2.Object.total_cajas[ll_Fila]
END IF

dw_1.Object.pprd_cancaj[il_Fila]	=	ll_Total_Cajas

RETURN
end subroutine

public function boolean duplicado (string as_columna, string as_valor);Long		ll_Fila
String	ls_Calibre

ls_Calibre	=	dw_2.Object.pprc_calini[il_Fila_det]

CHOOSE CASE as_Columna

	CASE "pprc_calini"
		ls_Calibre	=	as_Valor

END CHOOSE

ll_Fila = dw_2.Find("pprc_calini = '" + ls_Calibre + "'", 1, dw_2.RowCount())

IF ll_Fila > 0 AND ll_Fila <> il_Fila_det THEN
	MessageBox("Error", "Calibre ya fue ingresado anteriormente", Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

on w_mant_deta_programa_proc_cal.create
int iCurrent
call super::create
this.dw_2=create dw_2
this.pb_ins_det=create pb_ins_det
this.pb_eli_det=create pb_eli_det
this.rb_directo=create rb_directo
this.rb_porce=create rb_porce
this.st_1=create st_1
this.em_total=create em_total
this.gb_4=create gb_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_2
this.Control[iCurrent+2]=this.pb_ins_det
this.Control[iCurrent+3]=this.pb_eli_det
this.Control[iCurrent+4]=this.rb_directo
this.Control[iCurrent+5]=this.rb_porce
this.Control[iCurrent+6]=this.st_1
this.Control[iCurrent+7]=this.em_total
this.Control[iCurrent+8]=this.gb_4
end on

on w_mant_deta_programa_proc_cal.destroy
call super::destroy
destroy(this.dw_2)
destroy(this.pb_ins_det)
destroy(this.pb_eli_det)
destroy(this.rb_directo)
destroy(this.rb_porce)
destroy(this.st_1)
destroy(this.em_total)
destroy(this.gb_4)
end on

event ue_recuperadatos;w_main.SetMicroHelp("Recuperando Datos...")

SetPointer(HourGlass!)
PostEvent("ue_listo")

IF istr_mant.Agrega THEN
	pb_Cancela.Enabled	=	False
	pb_Primero.Enabled	=	False
	pb_Anterior.Enabled	=	False
	pb_Siguiente.Enabled	=	False
	pb_Ultimo.Enabled		=	False
	
	wf_nuevo()
	This.Title			= "INGRESO DE REGISTRO"
ELSE
	IF dw_1.RowCount() > 1 THEN
		pb_Primero.Enabled	=	True
		pb_Anterior.Enabled	=	True
		pb_Siguiente.Enabled	=	True
		pb_Ultimo.Enabled		=	True
	END IF
	
	il_fila	=	istr_mant.dw.GetRow()
	
	dw_1.SetRedraw(False)
	dw_1.SetRow(il_fila)
	dw_1.ScrollToRow(il_fila)
	dw_1.SetRedraw(True)

	IF istr_mant.Borra THEN
		dw_1.Enabled		=	False
		pb_Salir.Enabled	=	False
		This.Title			=	"ELIMINACION DE REGISTRO"
	ELSEIF istr_mant.Solo_Consulta THEN
		dw_1.Enabled			=	False
		dw_2.Enabled			=	False
		pb_Acepta.Enabled		=	False
		pb_Cancela.Enabled	=	False
		pb_ins_det.Enabled	=	False
		pb_eli_det.Enabled	=	False
		rb_porce.Enabled		=	False
		rb_directo.Enabled	=	False
		em_total.Enabled		=	False
		This.Title				=	"CONSULTA DE REGISTRO"
	ELSE
		pb_Salir.Enabled	=	False
		This.Title			=	"MANTENCION DE REGISTRO"
	END IF
END IF

dw_1.Object.plde_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[1])
dw_1.Object.espe_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[2])
dw_1.Object.ppre_numero[il_Fila]	=	Long(istr_Mant.Argumento[3])
dw_1.Object.reci_codigo[il_Fila]	=	Long(istr_Mant.Argumento[11])
dw_1.Object.clie_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[13])

IF istr_Mant.Argumento[9] = "1" THEN
	dw_1.Object.pprd_tipsel[il_Fila]		= 1
	dw_2.Object.pprc_calfin.Protect 		= 1
	dw_2.Object.b_seleccionacal.visible = 0
ELSE
	dw_1.Object.pprd_tipsel[il_Fila]		= 2
	dw_2.Object.pprc_calfin.Protect 		= 1
	dw_2.Object.b_seleccionacal.visible = 1
	dw_2.Object.pprc_calini.Protect 		= 1
END IF


dw_2.SetFilter("plde_codigo = " + istr_Mant.Argumento[1] + " and " + &
					"espe_codigo = " + istr_Mant.Argumento[2] + " and " + &
					"ppre_numero = " + istr_Mant.Argumento[3] + " and " + &
					"pprd_secuen = " + istr_Mant.Argumento[12])
dw_2.Filter()

IF dw_2.RowCount() > 0 THEN
	IF IsNull(dw_2.Object.pprc_pordis[1]) THEN
   	rb_directo.Checked 	= 	TRUE
		st_1.Visible			=	False
		em_total.Visible		=	False
		dw_2.Object.pprc_pordis.Protect = 1
		dw_2.Object.pprc_cancaj.Protect = 0
	ELSE
		rb_porce.Checked 		= 	TRUE
		st_1.Visible			=	TRUE
		em_total.Visible		=	TRUE
		dw_2.Object.pprc_pordis.Protect = 0
		dw_2.Object.pprc_cancaj.Protect = 1
		IF IsNull(istr_Mant.Argumento[10]) OR istr_Mant.Argumento[10] = "" THEN
			em_total.Text 		= 	"0"
		ELSE	
			em_total.Text		=	istr_Mant.Argumento[10]
			il_Total				=	Long(istr_Mant.Argumento[10])
		END IF	
	END IF	
END IF

IF dw_2.RowCount() > 0 THEN
	rb_porce.Enabled		=	False
	rb_directo.Enabled	=	False
	em_total.Enabled		=	False
	dw_1.Object.pprd_tipsel.Protect	=	1
END IF
end event

event ue_antesguardar();Integer		li_cont, il_row
Double		ld_sumaporce
String		ls_mensaje, ls_colu[]

IF dw_2.RowCount() = 0 THEN
	MessageBox("Error de Consistencia", "Falta registrar el Detalle de Calibres")
	dw_2.SetFocus()
	Message.DoubleParm = -1
	RETURN
END IF
	
IF Isnull(dw_2.Object.pprc_calini[il_fila_det]) OR dw_2.Object.pprc_calini[il_fila_det] = "" THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCalibre Inicial"
	ls_colu[li_cont]	= "pprc_calini"
END IF

IF rb_porce.Checked THEN
	ld_sumaporce = 0
	For il_row=1 to dw_2.rowcount()
	  ld_sumaporce = ld_sumaporce + dw_2.Object.pprc_pordis[il_row]
   NEXT
	IF Long(ld_sumaporce)<>100 THEN
	   MessageBox("Error de Consistencia", "El Porcentaje de Distribucion debe ser un 100%.", StopSign!, Ok!)
		Message.DoubleParm = -1
		RETURN
   END IF	
END IF	

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_2.SetColumn(ls_colu[1])
	dw_2.SetFocus()
	Message.DoubleParm = -1
	RETURN
END IF

end event

event open;Integer	li_especie,li_tipo,li_envase, li_Cliente

/* 
Argumentos
istr_Mant.Argumento[1]	=	Código Planta
istr_Mant.Argumento[2]	=	Especie
istr_Mant.Argumento[3]	=	Fecha
istr_Mant.Argumento[4]	=	Categoria
istr_Mant.Argumento[5]	=	Etiqueta
istr_Mant.Argumento[6]	=	Embalaje
istr_Mant.Argumento[7]	=	Tipo envase
istr_Mant.Argumento[8]	=	Envase
istr_Mant.Argumento[9]	=	Tipo Selección
istr_Mant.Argumento[10]	=	Cantidad de Cajas
istr_Mant.Argumento[11]	=	Código de Recibidor
istr_Mant.Argumento[12] =  Secuencia
istr_Mant.Argumento[13] =  Cliente
*/

x	= 100
y	= 450

This.Icon	=	Gstr_apl.Icono

PostEvent("ue_recuperadatos")

istr_Mant = Message.PowerObjectParm

li_especie	=	Integer(istr_Mant.Argumento[2])
li_tipo		=	Integer(istr_Mant.Argumento[7])
li_envase	=	Integer(istr_Mant.Argumento[8])
li_Cliente	=	Integer(istr_Mant.Argumento[13])

dw_1.SetTransObject(Sqlca)
istr_Mant.dw.ShareData(dw_1)

dw_1.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
IF idwc_especie.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Especies")
	idwc_especie.InsertRow(0)
ELSE
	idwc_especie.SetSort("espe_nombre A")
	idwc_especie.Sort()
END IF

dw_1.GetChild("reci_codigo", idwc_recibidor)
idwc_recibidor.SetTransObject(sqlca)
idwc_recibidor.Retrieve()

dw_1.GetChild("emba_codigo", idwc_embalaje)
idwc_embalaje.SetTransObject(sqlca)
idwc_embalaje.Retrieve()

dw_1.GetChild("etiq_codigo", idwc_etiqueta)
idwc_etiqueta.SetTransObject(sqlca)
idwc_etiqueta.Retrieve()

dw_2.GetChild("pprc_calini", idwc_calibre)
idwc_calibre.SetTransObject(sqlca)
idwc_calibre.Retrieve(li_especie,li_tipo,li_envase)

dw_1.Getchild("enva_codigo", idwc_envase)
idwc_envase.SetTransObject(sqlca)
idwc_envase.Retrieve(dw_1.Object.enva_tipoen[dw_1.GetRow()])

dw_2.Modify("datawindow.message.title='Error '+ is_titulo")
dw_2.SetRowFocusIndicator(Hand!)
dw_2.Modify("DataWindow.Footer.Height = 84")

dw_2.SetTransObject(Sqlca)
istr_Mant.dw2.ShareData(dw_2)

end event

event closequery;IF Not istr_mant.Borra THEN

	IF istr_mant.Agrega AND istr_mant.Respuesta <> 1 THEN 
		dw_1.DeleteRow(il_fila)
		dw_2.RowsMove(1,dw_2.RowCount(),Primary!,dw_2,1,Delete!)
		
		IF dw_1.RowCount() > 0 THEN
			dw_1.SelectRow(0, False)
			dw_1.SelectRow(dw_1.RowCount(), True)
		END IF
		
		RETURN
	END IF

	IF ib_Modifica AND istr_mant.Respuesta = 1 THEN
		This.TriggerEvent("ue_guardar")
		
		IF Message.DoubleParm = -1 THEN Message.ReturnValue = 1
		
		RETURN
	ELSEIF istr_mant.Respuesta = 2 THEN
		This.TriggerEvent("ue_deshace")
	END IF
END IF
end event

event ue_nuevo;IF ib_ok = False THEN RETURN

Integer	li_Secuencia

dw_1.Object.plde_codigo[il_Fila]			=	Integer(istr_Mant.Argumento[1])
dw_1.Object.espe_codigo[il_Fila]			=	Integer(istr_Mant.Argumento[2])
dw_1.Object.ppre_numero[il_Fila]			=	Long(istr_Mant.Argumento[3])
dw_1.Object.reci_codigo[il_Fila]			=	Long(istr_Mant.Argumento[11])

li_Secuencia									=	il_Fila
dw_2.Object.pprd_secuen[il_Fila_Det]	=  li_Secuencia

dw_2.SetRedraw(False)

dw_2.SetFilter("plde_codigo = " + istr_Mant.Argumento[1] + " and " + &
					"espe_codigo = " + istr_Mant.Argumento[2] + " and " + &
					"ppre_numero = " + istr_Mant.Argumento[3] + " and " + &
					"pprd_secuen = " + istr_Mant.Argumento[12])
dw_2.Filter()

dw_2.SetRedraw(True)
end event

event resize;Integer		li_posic_x, li_posic_y, li_Ancho = 300, li_Alto = 245, li_Siguiente = 255

This.Height			=	dw_1.Height + gb_4.Height + dw_2.Height + 300
This.Width			=	dw_1.width + 600

dw_1.x				=	78
dw_1.y				=	100
gb_4.y				=	dw_1.y + dw_1.Height + 20
dw_2.y				=	gb_4.y + gb_4.Height + 20

rb_porce.y			=	gb_4.y + ((gb_4.Height / 2) - 15)
st_1.y					=	gb_4.y + ((gb_4.Height / 2) - 15)
rb_directo.y			=	gb_4.y + ((gb_4.Height / 2) - 15)

li_posic_x			=	This.WorkSpaceWidth() - 400
li_posic_y			=	108

pb_acepta.width	=	li_Ancho
pb_acepta.height	=	li_Alto
pb_acepta.x			=	li_posic_x
pb_acepta.y			=	li_posic_y

pb_cancela.x		=	li_posic_x
pb_cancela.y		=	pb_acepta.y + li_Siguiente
pb_cancela.width	=	li_Ancho
pb_cancela.height	=	li_Alto

pb_salir.x			=	li_posic_x
pb_salir.y			=	pb_cancela.y + li_Siguiente
pb_salir.width		=	li_Ancho
pb_salir.height		=	li_Alto

pb_eli_det.x		=	li_posic_x
pb_ins_det.x	=	li_posic_x
pb_eli_det.y		=	(dw_2.y + dw_2.Height) - 255
pb_ins_det.y	=	pb_eli_det.y - 255
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_programa_proc_cal
boolean visible = false
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_programa_proc_cal
boolean visible = false
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_programa_proc_cal
boolean visible = false
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_programa_proc_cal
boolean visible = false
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_programa_proc_cal
integer x = 2747
integer y = 476
integer taborder = 50
end type

event pb_cancela::clicked;istr_mant.respuesta = 2

dw_2.SetFilter("")
dw_2.Filter()
	
CloseWithReturn(Parent, istr_mant)
end event

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_programa_proc_cal
integer x = 2747
integer y = 228
integer taborder = 40
boolean default = false
end type

event pb_acepta::clicked;Integer li_row 
Boolean lb_sale	=	True

istr_mant.respuesta = 1

li_row=1
Do While li_row<=dw_2.RowCount()
   IF IsNull(dw_2.Object.pprc_calini[li_row]) or dw_2.Object.pprc_calini[li_row]="" THEN
		MessageBox("Atención","Se Encuentra una Fila sin el Ingreso de Calibre Inicial.")
		li_row	=	dw_2.RowCount()
		lb_sale	=	False
	ELSE
		IF IsNull(dw_2.Object.pprc_cancaj[li_row]) or dw_2.Object.pprc_cancaj[li_row]=0 THEN
			MessageBox("Atención","Se Encuentra una Fila sin el Ingreso de Cantidad de Cajas.")
			li_row	=	dw_2.RowCount()
			lb_sale	=	False
		END IF
	END IF
	li_row++
Loop

IF lb_sale THEN
//	IF istr_mant.agrega THEN
		Parent.TriggerEvent("ue_nuevo")
//	ELSE
		CloseWithReturn(Parent, istr_mant)
//	END IF
END IF
end event

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_programa_proc_cal
integer x = 2747
integer y = 740
integer taborder = 60
end type

event pb_salir::clicked;dw_2.SetFilter("")
dw_2.Filter()
	
CALL SUPER::Clicked
end event

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_programa_proc_cal
integer x = 96
integer y = 48
integer width = 2432
integer height = 760
string dataobject = "dw_mues_program_proc_deta"
end type

event dw_1::itemchanged;call super::itemchanged;String  ls_Columna, ls_Nula
SetNull(ls_Nula)

ls_Columna = dwo.Name

CHOOSE CASE ls_Columna

	CASE "pprd_tipsel"
		IF Data = '1' THEN
			dw_2.Object.pprc_calini.Protect = 0
			dw_2.Object.pprc_calfin.Protect = 1
			dw_2.Object.b_seleccionacal.Visible = 0
		ELSE
			dw_2.Object.pprc_calini.Protect 	= 1
			dw_2.Object.pprc_calfin.Protect 	= 1
			dw_2.Object.b_seleccionacal.visible = 1
		END IF

END CHOOSE
end event

type dw_2 from uo_dw within w_mant_deta_programa_proc_cal
integer x = 96
integer y = 1020
integer width = 2432
integer height = 832
integer taborder = 20
boolean bringtotop = true
boolean titlebar = true
string title = "Detalle de Lote"
string dataobject = "dw_mant_prograproceso_cal"
boolean hscrollbar = true
end type

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila_det = Row
END IF

RETURN 0
end event

event itemerror;call super::itemerror;RETURN 1
end event

event losefocus;call super::losefocus;Accepttext()
end event

event rowfocuschanged;call super::rowfocuschanged;ib_datos_ok = True

IF This.RowCount() < 1 OR CurrentRow = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila_det = CurrentRow
END IF
end event

event itemchanged;String  		ls_Columna, ls_Nula
Decimal{2}  ld_porce

SetNull(ls_Nula)

ls_Columna = dwo.Name

ib_modifica = True

CHOOSE CASE ls_Columna

	CASE "pprc_calini"
		IF NoExisteCalibre(Data) OR Duplicado(ls_Columna, Data) THEN
			This.SetItem(il_fila_det, ls_Columna, ls_Nula)
			RETURN 1
		ELSE
			This.SetItem(il_fila_det, "pprc_calfin", Data)
		END IF

	CASE "pprc_pordis"	
		IF Dec(data) >= 100 THEN
			MessageBox("Atención", "Porcentaje de Distribución no puede ser superior a 99.99 %")
			This.SetItem(row,ls_Columna, Dec(ls_Nula))
			RETURN 1
		ELSE
			ld_porce = Dec(data)/100
			This.Object.pprc_cancaj[il_fila_det] = Round(ld_porce * il_total,0)
			Captura_Totales()
		END IF
		
	CASE "pprc_cancaj"
		Captura_Totales()

END CHOOSE
end event

event buttonclicked;Str_busqueda	lstr_busq
Integer 			il_final

CHOOSE CASE dwo.Name
	
	CASE "b_seleccionacal"
		
		lstr_busq.argum[1]	=	istr_Mant.argumento[2]
		lstr_busq.argum[2]	=	String(dw_1.Object.enva_tipoen[dw_1.GetRow()])//istr_Mant.argumento[7]
		lstr_busq.argum[3]	=	String(dw_1.Object.enva_codigo[dw_1.GetRow()])//istr_Mant.argumento[8]
		lstr_busq.argum[4]	=	istr_Mant.argumento[13]

		OpenWithParm(w_sele_calibresenvase, lstr_busq)

		lstr_busq	= Message.PowerObjectParm

		IF lstr_busq.argum[1] <> "" THEN
			
			
			IF Duplicado("pprc_calini",lstr_busq.argum[5])=FALSE THEN
				il_final = Integer(lstr_busq.argum[4])

				IF il_final = 1 THEN
					dw_2.Object.pprc_calini[il_fila_det]			=	lstr_busq.argum[5]
					dw_2.Object.pprc_calfin[il_fila_det]			=	lstr_busq.argum[5]
				END IF	

				IF il_final > 1 THEN
					IF dw_1.Object.pprd_tipsel[il_Fila] = 1 THEN
						dw_2.Object.pprc_calini[il_fila_det]	=	lstr_busq.argum[5]
						dw_2.Object.pprc_calfin[il_fila_det]	=	lstr_busq.argum[5]
					ELSE
						dw_2.Object.pprc_calini[il_fila_det]	=	lstr_busq.argum[5]
						dw_2.Object.pprc_calfin[il_fila_det]	=	lstr_busq.argum[il_final]
					END IF

				END IF
			END IF	
		END IF
 		
RETURN

END CHOOSE
end event

type pb_ins_det from picturebutton within w_mant_deta_programa_proc_cal
integer x = 2747
integer y = 1260
integer width = 302
integer height = 244
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Signo Mas.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Signo Mas-bn.png"
alignment htextalign = left!
end type

event clicked;Parent.TriggerEvent("ue_nuevo_detalle")
end event

type pb_eli_det from picturebutton within w_mant_deta_programa_proc_cal
integer x = 2747
integer y = 1512
integer width = 302
integer height = 244
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Signo Menos.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Signo Menos-bn.png"
alignment htextalign = left!
end type

event clicked;Parent.TriggerEvent("ue_borra_detalle")
end event

type rb_directo from radiobutton within w_mant_deta_programa_proc_cal
integer x = 1957
integer y = 880
integer width = 416
integer height = 80
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Ing. Directo"
boolean checked = true
end type

event clicked;Integer	li_especie, li_tipo, li_envase
Long		ll_Fila
String	ls_Null

SetNull(ls_Null)

st_1.Visible 			=	FALSE
em_total.Visible		=	FALSE

dw_2.Enabled 			= 	TRUE

li_especie	=	Integer(istr_Mant.Argumento[2])
li_tipo		=	Integer(istr_Mant.Argumento[7])
li_envase	=	Integer(istr_Mant.Argumento[8])

dw_2.GetChild("pprc_calini", idwc_calibre)
idwc_calibre.SetTransObject(sqlca)
IF idwc_calibre.Retrieve(li_especie,li_tipo,li_envase) = 0 THEN
	MessageBox("Atención","No se han Registrados Calibres para Especie indicada")
ELSE
	idwc_calibre.InsertRow(0)
END IF

pb_ins_det.Enabled 	=	TRUE
pb_eli_det.Enabled	=	TRUE

dw_2.Object.pprc_pordis.Protect = 1
dw_2.Object.pprc_cancaj.Protect = 0

FOR ll_Fila	=	1 TO dw_2.RowCount()
	dw_2.Object.pprc_pordis[ll_Fila]	=	Dec(ls_Null)
NEXT

dw_2.SetFocus()
end event

type rb_porce from radiobutton within w_mant_deta_programa_proc_cal
integer x = 393
integer y = 880
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Porcentual"
end type

event clicked;Integer	li_especie,li_tipo, li_envase
Long		ll_Fila
String	ls_Null

SetNull(ls_Null)

st_1.Visible 			=	TRUE
em_total.Visible		=	TRUE

li_especie	=	Integer(istr_Mant.Argumento[2])
li_tipo		=	Integer(istr_Mant.Argumento[7])
li_envase	=	Integer(istr_Mant.Argumento[8])

dw_2.GetChild("pprc_calini", idwc_calibre)
idwc_calibre.SetTransObject(sqlca)
IF idwc_calibre.Retrieve(li_especie,li_tipo,li_envase) = 0 THEN
	MessageBox("Atención","No se han Registrados Calibres para Especie indicada")
ELSE
	idwc_calibre.InsertRow(0)
END IF

pb_ins_det.Enabled 	=	FALSE
pb_eli_det.Enabled	=	FALSE

dw_2.Object.pprc_pordis.Protect = 0
dw_2.Object.pprc_cancaj.Protect = 1

FOR ll_Fila	=	1 TO dw_2.RowCount()
	dw_2.Object.pprc_cancaj[ll_Fila]	=	Long(ls_Null)
NEXT

em_total.SetFocus()
end event

type st_1 from statictext within w_mant_deta_programa_proc_cal
boolean visible = false
integer x = 896
integer y = 888
integer width = 366
integer height = 64
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Total Cajas"
boolean focusrectangle = false
end type

type em_total from editmask within w_mant_deta_programa_proc_cal
boolean visible = false
integer x = 1275
integer y = 880
integer width = 402
integer height = 84
integer taborder = 50
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "#######"
end type

event modified;dw_2.Enabled 			= TRUE
pb_ins_det.Enabled	= TRUE
pb_eli_det.Enabled	= TRUE

il_total= Long(em_total.text)

RedistribuyeCalibres()
end event

type gb_4 from groupbox within w_mant_deta_programa_proc_cal
integer x = 96
integer y = 820
integer width = 2432
integer height = 188
integer taborder = 40
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
string text = "Distribución"
end type

