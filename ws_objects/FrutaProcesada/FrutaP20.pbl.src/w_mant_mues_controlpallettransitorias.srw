$PBExportHeader$w_mant_mues_controlpallettransitorias.srw
forward
global type w_mant_mues_controlpallettransitorias from w_mant_tabla
end type
type st_1 from statictext within w_mant_mues_controlpallettransitorias
end type
type st_2 from statictext within w_mant_mues_controlpallettransitorias
end type
type dw_10 from datawindow within w_mant_mues_controlpallettransitorias
end type
type dw_11 from datawindow within w_mant_mues_controlpallettransitorias
end type
end forward

global type w_mant_mues_controlpallettransitorias from w_mant_tabla
integer width = 3634
integer height = 1872
string title = "Recepción Definitiva Masiva"
event ue_validapassword ( )
st_1 st_1
st_2 st_2
dw_10 dw_10
dw_11 dw_11
end type
global w_mant_mues_controlpallettransitorias w_mant_mues_controlpallettransitorias

type variables
Long li_NroRecepcion[20]

DataWindowChild	idwc_cliente, idwc_planta
uo_responablescierre	iuo_responablescierre

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
	SELECT	Count(*), Now()
		INTO	:li_Contador, :ldt_FechaHora
		FROM	dba.admasistemas;	
ELSE
	SELECT	Count(*), GetDate()
		INTO	:li_Contador, :ldt_FechaHora
		FROM	dba.admasistemas;
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
		
		UPDATE dba.DESPAFRIGOEN SET
		defe_estado = 0 WHERE
		clie_codigo = :li_cliente AND
		plde_codigo = :li_planta AND
		defe_numero = :ll_numero;
		
		UPDATE dba.controldespachos SET
		code_gemsaa = li_nulo,
		code_gengde = li_nulo,
		code_gengde = li_nulo,
		code_demade = li_nulo WHERE
		clie_codigo = :li_cliente AND
		plde_codigo = :li_planta AND
		defe_numero = :ll_numero;
	END IF	
NEXT	
end subroutine

protected function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_1.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False
//actualizadespacho()

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF dw_1.Update(True, False) = 1 then 
	Commit;
	
	IF sqlca.SQLCode <> 0 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		lb_Retorno	=	False
	ELSE
		lb_Retorno	=	True
		actualizadespacho()	
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

on w_mant_mues_controlpallettransitorias.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.dw_10=create dw_10
this.dw_11=create dw_11
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.dw_10
this.Control[iCurrent+4]=this.dw_11
end on

on w_mant_mues_controlpallettransitorias.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.dw_10)
destroy(this.dw_11)
end on

event open;String ls_usurio
Time lt_hora
Date ld_fecha

x				= 0
y				= 0
//This.Width	= dw_1.width + 440
//This.Height	= 1993
im_menu	= m_principal

This.ParentWindow().ToolBarVisible	= True
im_menu.Item[1].Item[6].Enabled		= True
im_menu.Item[7].Visible					= True

dw_1.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.dw	= dw_1

dw_10.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_10.InsertRow(0)
dw_10.SetItem(1,"clie_codigo",gi_codexport)

dw_11.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_11.InsertRow(0)
dw_11.SetItem(1,"plde_codigo",gi_codplanta)

ls_usurio  = gstr_us.nombre
ld_fecha = date(now()) 
lt_hora = Time(fechahora())

istr_mant.argumento[3] = ls_usurio
istr_mant.argumento[5] = String(ld_fecha)
istr_mant.argumento[6] = String(lt_hora)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)
							
//PostEvent("ue_validapassword")

istr_mant.argumento[1]	=	String(gi_codexport)
istr_mant.argumento[2]	=	String(gi_codplanta)

iuo_responablescierre	=	CREATE uo_responablescierre

/*
Chequea Tipo Usuario
*/

IF Not iuo_responablescierre.Existe(gi_codplanta,gstr_us.Nombre,False,Sqlca) THEN
  MessageBox("Atención","Usuario No está autorizado para realizar Traspasos de Recepción a Definitivo")
  Close(This)
END IF
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta

DO
	ll_fila	= dw_1.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]))
	
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

event ue_imprimir;SetPointer(HourGlass!)

Long		fila, ll_Fila
String		ls_NroRecepcion

istr_info.titulo	= "CONTROL DE RECEPCION"
istr_info.copias	= 1

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_recepcion_pallet_trans_def"

vinf.dw_1.SetTransObject(sqlca)

FOR ll_Fila = 1 TO dw_1.RowCount()
	IF dw_1.Object.aplica[ll_Fila] = 1 THEN
		IF ls_NroRecepcion = '' THEN
			ls_NroRecepcion = String(dw_1.Object.rfpe_numero[ll_Fila])
		ELSE
			ls_NroRecepcion = ls_NroRecepcion + ',' + String(dw_1.Object.rfpe_numero[ll_Fila])
		END IF
	END IF
NEXT

fila = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[1]) , Integer(istr_mant.argumento[2]), ls_NroRecepcion)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
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

event ue_guardar;Integer 	li_Planta, li_Cliente, li_administradora, li_bodeadmin, li_codigo, li_Estado, li_gencaj, &
			li_plantaori, li_seleccion = 0, li_area, li_entrada, li_tipopa
Long    	ll_Recepcion, ll_Fila, ll_numero, ll_cajas
String	ls_usuario, ls_cajas, ls_null
Date		ld_fecha
Time		lt_hora

IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

SetNull(ls_null)
w_main.SetMicroHelp("Grabando información...")

FOR ll_Fila = 1 TO dw_1.RowCount()
	
	IF dw_1.Object.Aplica[ll_Fila] = 1 THEN
					
			li_Planta  		= dw_1.Object.plde_codigo[ll_Fila]
			li_cliente 		= dw_1.Object.clie_codigo[ll_Fila]
			ll_Recepcion  	= dw_1.Object.rfpe_numero[ll_Fila]
						
			DECLARE GrabaPallet PROCEDURE FOR dba.FProc_CargaRecepciones_Masiva
				@Cliente 	=:li_Cliente,
				@Planta  	=:li_Planta,
				@Recepcion	= :ll_Recepcion,
				@seleccion 	= :li_seleccion;		
			EXECUTE GrabaPallet;
						
			IF sqlca.sqlcode < 0 THEN
				F_ErrorBaseDatos(sqlca,"El proceso Carga FProc_CargaRecepciones_Transmitidas " +&
				" no se ejecutó Exitosamente")
				Return	
			ELSE
			
				ls_usuario 	= dw_1.Object.rfpe_usuari[ll_Fila]
				ld_fecha		= dw_1.Object.rfpe_fecrec[ll_Fila]
				lt_hora		= dw_1.Object.rfpe_horrec[ll_Fila]
				ll_numero 	= dw_1.Object.rfpe_numero[ll_Fila]
				li_NroRecepcion[ll_Fila] = ll_Recepcion
				/*
				Rescata Area Responsable del Usuario
				*/
				SELECT area_codigo
				INTO  :li_area
				FROM dba.spro_responsablecierre
				WHERE usua_codigo = :ls_usuario;
				/*
				Actualiza Control de Recepciones, Estado Traspasado 
				*/
				UPDATE dba.spro_controlrecepciones set
					spcr_estado = 1,
					spcr_usuare = :ls_usuario,
					area_codigo = :li_area ,
					spcr_feccie = :ld_fecha,
					spcr_horcie = :lt_hora
				WHERE clie_codigo = :li_cliente
				AND plde_codigo = :li_Planta
				AND rfpe_numero = :ll_numero;	
				Commit;	
				/*
				Actualiza Recepciones, Estado Traspasado 
				*/
				UPDATE dba.recfruprocee_trans  SET
					RFPE_ESTADO = 2
					WHERE clie_codigo = :li_Cliente AND  
							plde_codigo = :li_Planta  AND  
							rfpe_numero = :ll_Numero;
				 Commit;
			END IF			
		END IF
	NEXT
		IF sqlca.sqlcode < 0 THEN
		F_ErrorBaseDatos(sqlca,"El proceso Carga Pallet Por Recepción Transmitida " +&
			" no se ejecutó Exitosamente")
		Close GrabaPallet;
		Return		  
	ELSE			
		 Close GrabaPallet;		 
		 MessageBox("Atención", "El proceso Carga Pallet Por Recepción Transmitida " +&
							" se ejecutó Exitosamente")			 
	END IF 
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_controlpallettransitorias
integer x = 82
integer y = 360
integer width = 3063
integer height = 1324
string dataobject = "dw_mues_recfruprocee_trans_masiva"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

event dw_1::doubleclicked;//
end event

event dw_1::clicked;Integer I, marca

IF Row > 0 THEN
 il_fila = Row
END IF

String ls_Columna

ls_Columna = dwo.name

CHOOSE CASE ls_Columna
  
 CASE "aplicatodos"
		IF This.RowCount() > 0 THEN
			IF	This.Object.AplicaTodos[1] = 1 THEN
				This.Object.AplicaTodos[1] = 0
				 marca = 0
			ELSE
					This.Object.AplicaTodos[1] = 1
				 marca = 1
			END IF
		
			FOR I=1 TO This.RowCount()
					This.Object.aplica[I] = marca
			NEXT
		END IF
		
 CASE "aplica"
		This.Object.AplicaTodos[1] = 0
	
END CHOOSE

RETURN 0
end event

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_controlpallettransitorias
integer x = 82
integer y = 40
integer width = 3063
integer height = 284
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_controlpallettransitorias
integer x = 3246
integer taborder = 20
end type

event pb_lectura::clicked;call super::clicked;dw_10.Enabled	=	FALSE
dw_10.Modify("clie_codigo.background.color = " + string(RGB(166,180,210)))

dw_11.Enabled	=	FALSE
dw_11.Modify("plde_codigo.background.color = " + string(RGB(166,180,210)))

end event

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_controlpallettransitorias
integer x = 3246
integer taborder = 40
end type

event pb_nuevo::clicked;call super::clicked;dw_10.Enabled	=	TRUE
dw_10.Modify("clie_codigo.background.color = " + string(rgb(252,252,252)))

dw_11.Enabled	=	TRUE
dw_11.Modify("plde_codigo.background.color = " + string(rgb(252,252,252)))


//istr_mant.argumento[1] = String(dw_especies.GetItemNumber(dw_especies.GetRow(),"espe_codigo"))

end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_controlpallettransitorias
boolean visible = false
integer x = 3246
integer taborder = 50
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_controlpallettransitorias
boolean visible = false
integer x = 3246
integer taborder = 60
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_controlpallettransitorias
integer x = 3246
integer taborder = 70
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_controlpallettransitorias
integer x = 3246
integer taborder = 80
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_controlpallettransitorias
integer x = 3246
integer taborder = 90
end type

type st_1 from statictext within w_mant_mues_controlpallettransitorias
integer x = 485
integer y = 200
integer width = 247
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within w_mant_mues_controlpallettransitorias
integer x = 485
integer y = 88
integer width = 247
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_10 from datawindow within w_mant_mues_controlpallettransitorias
integer x = 763
integer y = 84
integer width = 1230
integer height = 96
integer taborder = 20
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String		ls_Columna[]
Integer 		li_null
SetNull(li_null)

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	IF F_ValidaCliente(Integer(data)) THEN
		istr_mant.argumento[1] = data
		istr_mant.argumento[2]	=	String(dw_11.Object.plde_codigo[1])
		dw_11.SetItem(1, "plde_codigo", Integer(istr_mant.argumento[2]))
		idwc_planta.Retrieve(1)
	ELSE
		dw_10.SetItem(Row, "clie_codigo", li_null)
		RETURN 1
	END IF
ELSE
	dw_10.SetItem(Row, "clie_codigo", li_null)
	RETURN 1
END IF

end event

event itemerror;Return 1
end event

type dw_11 from datawindow within w_mant_mues_controlpallettransitorias
integer x = 763
integer y = 188
integer width = 969
integer height = 100
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer 		li_null
String		ls_Columna[]
SetNull(li_null)

IF ExistePlanta(Integer(istr_mant.Argumento[1]), Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[2] = data
ELSE
	dw_11.SetItem(Row, "plde_codigo", li_null)
	RETURN 1
END IF
end event

event itemerror;Return 1
end event

