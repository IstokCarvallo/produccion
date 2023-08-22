$PBExportHeader$w_maed_repalletenca_cambiofolio.srw
forward
global type w_maed_repalletenca_cambiofolio from w_mant_encab_deta_csd
end type
end forward

global type w_maed_repalletenca_cambiofolio from w_mant_encab_deta_csd
integer width = 3429
integer height = 2044
string title = "CAMBIO DE FOLIO"
string menuname = ""
boolean maxbox = false
event ue_imprimir ( )
end type
global w_maed_repalletenca_cambiofolio w_maed_repalletenca_cambiofolio

type variables
w_mant_deta_repalletdeta_salida iw_mantencion

DataWindowChild	dw_planta, dw_plades, dw_puerto,dw_etiquetas,dw_plantas,dw_clientes,dw_ptodes
end variables

forward prototypes
public subroutine habilitaingreso (string columna)
public function boolean noexistecliente (integer ai_codigo)
public subroutine habilitaencab (boolean habilita)
public subroutine cuentatarjas ()
public function boolean existefolio (string as_columna, string as_valor)
end prototypes

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "RECEPCION DE PALLETS"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_repaletizado"

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[1]), &
									Long(istr_mant.argumento[2]), &
									Integer(istr_mant.argumento[3]),-1)

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

public subroutine habilitaingreso (string columna);Date	ld_fecha
Integer  li_tarjas, li_tardef
Boolean	lb_estado = True

dw_2.AcceptText()

li_tarjas = dw_2.Object.repe_cantar[1]
li_tardef =	dw_2.Object.repe_tardef[1]

IF IsNull(li_tarjas) THEN li_tarjas = 0
IF IsNull(li_tardef) THEN li_tardef = 0

IF dw_2.RowCount() > 0 THEN
	IF IsNull(dw_2.Object.plde_codigo[1]) OR dw_2.Object.plde_codigo[1] = 0 OR &
		IsNull(dw_2.Object.repe_numero[1]) OR dw_2.Object.repe_numero[1] = 0 OR &
		IsNull(dw_2.Object.repe_fecrep[1]) OR dw_2.Object.repe_fecrep[1] = ld_fecha OR &
		li_tarjas + li_tardef = 0 THEN
		lb_estado = False
	END IF
END IF

pb_ins_det.Enabled = lb_estado
end subroutine

public function boolean noexistecliente (integer ai_codigo);String	ls_nombre

SELECT	clie_nombre
	INTO	:ls_nombre  
   FROM	dba.clientesprod  
   WHERE	clie_codigo =	:ai_codigo;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Clientes Producción")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Error", "Cliente no Existe. Ingrese otro.")
	RETURN True
END IF

IF F_ValidaCliente(ai_codigo) THEN
	istr_mant.Argumento[3]	=	String(ai_codigo)
	RETURN False
ELSE
	RETURN True
END IF


end function

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.SetTabOrder("clie_codigo",1)	
	dw_2.SetTabOrder("plde_codigo",10)
	dw_2.SetTabOrder("repe_numero",20)
	dw_2.Modify("repe_numero.BackGround.Color = " + String(rgb(255,255,255)))
	dw_2.Modify("plde_codigo.BackGround.Color = " + String(rgb(255,255,255)))
	dw_2.Modify("clie_codigo.BackGround.Color = " + String(rgb(255,255,255)))	
	dw_2.SetColumn("repe_numero")
	dw_2.SetFocus()
ELSE
	dw_2.SetTabOrder("repe_numero",0)
	dw_2.SetTabOrder("plde_codigo",0)
	dw_2.SetTabOrder("clie_codigo",0)
	dw_2.Modify("repe_numero.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("plde_codigo.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("clie_codigo.BackGround.Color = " + String(RGB(166,180,210)))
END IF
end subroutine

public subroutine cuentatarjas ();Long I,ll_tra=0,ll_def=0, ll_cantar, ll_tardef

FOR I=1 TO dw_1.Rowcount()
	IF dw_1.Object.paen_tipopa[I]=1 THEN
		ll_def ++
	ELSE
		ll_tra ++
	END IF
NEXT

ll_cantar	=	dw_2.Object.repe_cantar[1]
ll_tardef	=	dw_2.Object.repe_tardef[1]

istr_mant.argumento[10]	= String(ll_cantar + ll_tardef)
istr_mant.argumento[12]	= String(dw_2.Object.repe_cantar[1] )
istr_mant.argumento[11]	= String(dw_2.Object.repe_tardef[1] )
		
RETURN
end subroutine

public function boolean existefolio (string as_columna, string as_valor);Integer	li_planta, li_existe, li_tipopa, li_cliente
Long		ll_nfolio

li_cliente	=	dw_2.Object.clie_codigo[1]
li_planta	=	dw_2.Object.plde_codigo[1]
ll_nfolio 	=	dw_2.Object.repe_numero[1]

CHOOSE CASE as_columna
	CASE "clie_codigo"
		li_cliente	=	Integer(as_valor)
		
	CASE "plde_codigo"
		li_planta	=	Integer(as_valor)
		
	CASE "repe_numero"
		ll_nfolio 	=	Long(as_valor)
		
END CHOOSE

SELECT	repe_tipopa,Count(*) 
	INTO	 :li_tipopa, :li_existe
	FROM	dba.REPALLETENCA
	WHERE	plde_codigo	=	:li_planta
	AND	repe_numero	=	:ll_nfolio
	AND   clie_codigo =  :li_cliente
	GROUP BY repe_tipopa;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Repalletenca")
	RETURN True
ELSE
			IF li_existe > 0 THEN
				IF li_tipopa <> 6 THEN
					Messagebox("Advertencia","Número de Repalletizado No Corresponde a Cambio de Folio")
					RETURN True
				END IF
				istr_mant.argumento[1]	=	String(li_planta)
				istr_mant.argumento[2]	=	String(ll_nfolio)
				istr_mant.argumento[3]	=	String(li_cliente)
				
				This.TriggerEvent("ue_recuperadatos")

				istr_mant.argumento[4]	=	String(dw_2.Object.repe_cantar[1])
				RETURN False
			ELSE
				istr_mant.argumento[1]	=	String(li_planta)
				istr_mant.argumento[2]	=	String(ll_nfolio)
				RETURN False
			END IF
		   RETURN False
END IF

end function

event open;call super::open;/*
  istr_mant.argumento[1]	= li_planta
  istr_mant.argumento[2]	= nfolio
  istr_mant.argumento[3]	= Cliente
  istr_mant.argumento[4]	= Tarjas transitorias
  istr_mant.argumento[5]	= Mantencion
  istr_mant.argumento[6]	= Tarjas definitivas
  istr_mant.argumento[8]   = Lista de Productores de Folios a Repalletizar
  istr_mant.argumento[11]	= Puchos Ingresados
  istr_mant.argumento[12]	= Pallet Ingresados  
  istr_mant.argumento[13]  = Nro. de Pallet
  istr_mant.argumento[14]  = Tipo de Pantalla; 1 para el repalletizado
  istr_mant.argumento[25]  = Valor de Tipo de Repaletizado
*/
IF gi_CodExport = 300 THEN dw_2.Object.plde_codigo.dddw.Name	=	"dw_mues_plantadesp"

dw_2.GetChild("plde_codigo", dw_planta)
dw_planta.SetTransObject(sqlca)
dw_planta.Retrieve(1)

dw_2.GetChild("clie_codigo", dw_clientes)
dw_clientes.SetTransObject(sqlca)
dw_clientes.Retrieve(gi_CodExport)

buscar	= "Pallet Nuevo:Npaen_numero,Descripción:Svari_nombre"
ordenar	= "Pallet Nuevo:paen_numero,Descripción:vari_nombre"

istr_mant.argumento[3]	=	String(gi_CodExport)
istr_mant.argumento[5]	=	'dw_mues_repalletdeta_salida_cambiofolio'
istr_mant.argumento[24] =	''
istr_mant.argumento[14] =  '1'
istr_mant.argumento[25] = 	'6'

end event

event ue_borra_detalle;call super::ue_borra_detalle;IF dw_1.rowcount() < 2 THEN
	dw_1.SetFocus()
	RETURN
END IF

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

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
		HabilitaEncab(True)
		pb_eli_det.Enabled = False
	END IF
END IF

istr_mant.borra	 = False
end event

event ue_nuevo_detalle;call super::ue_nuevo_detalle;Long		ll_cantar, ll_tardef

istr_mant.borra	= False
istr_mant.agrega	= True

ll_cantar	=	dw_2.Object.repe_cantar[1]
ll_tardef	=	dw_2.Object.repe_tardef[1]

IF dw_1.RowCount() >= (ll_cantar + ll_tardef ) THEN
	MessageBox("Atención", "No puede ingresar más Tarjas.")
ELSE

	istr_mant.Argumento[7]	=	"0"
	istr_mant.Argumento[16]	=	"0"
	istr_mant.Argumento[21]	=	String(dw_2.Object.plde_codigo[1])
	
	CuentaTarjas()		
	
	OpenWithParm(iw_mantencion, istr_mant)
	
	IF dw_1.RowCount() > 0 THEN HabilitaEncab(False)
	
	IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
		pb_eliminar.Enabled	= TRUE
		pb_grabar.Enabled		= TRUE
	END IF
		
	dw_1.SetRow(il_fila)
	dw_1.SelectRow(il_fila,True)
END IF

end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta
DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	ll_fila_e	= dw_2.Retrieve(Integer(istr_mant.argumento[1]), Long(istr_mant.argumento[2]),Integer(istr_mant.argumento[3]))

	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		DO
//			ll_fila_d	= dw_3.Retrieve(Integer(istr_mant.argumento[1]), Long(istr_mant.argumento[2]))
			ll_fila_d	= dw_1.Retrieve(Integer(istr_mant.argumento[1]), Long(istr_mant.argumento[2]),Integer(istr_mant.argumento[3]))

			IF ll_fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				pb_eliminar.Enabled	= True
				pb_grabar.Enabled		= True
				pb_imprimir.Enabled		= True
				pb_ins_det.Enabled	= True

				IF ll_fila_e > 0 THEN
					istr_mant.argumento[25] = String(dw_2.Object.repe_tipopa[1])
				END IF
				
				IF ll_fila_d > 0 THEN
				   pb_eli_det.Enabled	= True
					dw_1.SetRow(1)
					dw_1.SelectRow(1,True)
					dw_1.SetFocus()
					HabilitaEncab(False)
					IF ll_fila_d = dw_2.Object.repe_cantar[1] THEN pb_ins_det.Enabled = False
				ELSE
					pb_ins_det.SetFocus()
				END IF
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)

dw_2.Enabled	=	False
end event

on w_maed_repalletenca_cambiofolio.create
call super::create
end on

on w_maed_repalletenca_cambiofolio.destroy
call super::destroy
end on

event ue_nuevo;call super::ue_nuevo;HabilitaEncab(True)

dw_2.SetItem(1, "clie_codigo", gi_CodExport)
dw_2.SetItem(1, "plde_codigo", gi_CodPlanta)

istr_mant.argumento[1]	=	String(gi_CodPlanta)
istr_mant.argumento[3]	=	String(gi_CodExport)


end event

event ue_seleccion;call super::ue_seleccion;Long		ll_null

SetNull(ll_null)

istr_busq.argum[1]	=	String(dw_2.Object.clie_codigo[1])
istr_busq.argum[2]	=	String(dw_2.Object.plde_codigo[1])
istr_busq.argum[3]	=	'1'

OpenWithParm(w_busc_repalletenca, istr_busq)

istr_busq	=	Message.PowerObjectParm

IF istr_busq.argum[5] <> "" THEN
	istr_mant.argumento[1]	= istr_busq.argum[2]
	istr_mant.argumento[2]	= istr_busq.argum[5]
	istr_mant.argumento[3]  = istr_busq.argum[1]
	IF	istr_mant.argumento[2]	<>	""	THEN
		dw_2.SetItem(1, "repe_numero", Long(istr_mant.argumento[2]))
		IF ExisteFolio('repe_numero', String(dw_2.Object.repe_numero[1])) THEN
			dw_2.SetFocus()
			dw_2.SetColumn("repe_numero")
			dw_2.SetItem(1, "repe_numero", ll_null)			
			RETURN
		END IF
	ELSE
		This.TriggerEvent("ue_recuperadatos")
	END IF
ELSE
	pb_buscar.SetFocus()
END IF
end event

event ue_modifica_detalle;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega			= False
	istr_mant.borra			= False
	istr_mant.Argumento[6]	=	String(dw_1.Object.paen_numero[il_fila])

	OpenWithParm(w_maed_palletencab_consulta, istr_mant)
END IF
end event

event ue_antesguardar;call super::ue_antesguardar;Long		ll_Fila
Integer	li_Fila = 0

IF dw_2.GetNextModified(0, Primary!) > 0 THEN
	dw_2.SetItem(1, "repe_fecact", Today())
	dw_2.SetItem(1, "repe_horact", Now())
END IF

FOR ll_Fila = 1 TO dw_1.RowCount()
	 li_Fila++
	 dw_1.Object.pafr_secuen[ll_Fila]	=	1
	
NEXT
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_repalletenca_cambiofolio
integer x = 37
integer y = 948
integer width = 2757
integer height = 932
integer taborder = 100
string title = "Detalle de Pallets"
string dataobject = "dw_mues_repalletdeta_salida_cambiofolio"
boolean livescroll = false
end type

event dw_1::retrieveend;If RowCount() > 0 Then	pb_ins_det.Enabled = TRUE
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_repalletenca_cambiofolio
integer x = 41
integer y = 36
integer width = 2734
integer height = 880
string dataobject = "dw_mant_repalletenca"
end type

event dw_2::itemchanged;call super::itemchanged;Long		ll_null
String	ls_columna
Date		ld_nula

SetNull(ll_null)
SetNull(ld_nula)
ls_columna = dwo.Name

CHOOSE CASE ls_columna
		
	CASE "plde_codigo"
		IF ExisteFolio(ls_columna, data) THEN
			This.SetItem(Row, ls_Columna, ll_null)
			RETURN 1
		END IF			
		
	CASE "repe_numero"
		IF ExisteFolio(ls_columna, data) THEN
			This.SetItem(Row, ls_Columna, ll_null)
			RETURN 1
		END IF			
		
	CASE "repe_cantar"
		istr_mant.argumento[4]	= data

   CASE "repe_tardef"
		istr_mant.argumento[6]	= data
		
	CASE "clie_codigo"
		IF NoExisteCliente(Integer(data)) THEN
			This.SetItem(Row, ls_Columna, gi_codexport)
			RETURN 1
		ELSE
			istr_mant.argumento[3]=data
         dw_2.GetChild("plde_codigo", dw_planta)
			dw_planta.SetTransObject(sqlca)
			dw_planta.Retrieve(1)
		END IF	

	CASE "repe_tipopa"
		IF integer(data) <> 6 THEN
			Messagebox("Advertencia","Tipo de Repalletizado No Disponible En Esta Opción!")
			Parent.TriggerEvent("ue_nuevo")
		END IF
		
	CASE "repe_fecrep"
		IF Not f_validafechatempo(date(data)) THEN
			This.SetItem(Row, ls_Columna, ld_nula)
			RETURN 1
		END IF		
		
			
END CHOOSE

HabilitaIngreso(ls_columna)

end event

event dw_2::doubleclicked;//
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_repalletenca_cambiofolio
integer x = 3150
integer y = 284
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_repalletenca_cambiofolio
integer x = 3150
integer y = 512
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_repalletenca_cambiofolio
integer x = 3150
integer y = 728
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_repalletenca_cambiofolio
integer x = 3150
integer y = 960
boolean enabled = true
boolean default = true
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_repalletenca_cambiofolio
integer x = 3150
integer y = 1188
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_repalletenca_cambiofolio
integer x = 3150
integer y = 1516
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_repalletenca_cambiofolio
integer x = 3150
integer y = 1692
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_repalletenca_cambiofolio
integer x = 3150
integer y = 104
end type

