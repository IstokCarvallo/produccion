$PBExportHeader$w_maed_cambioaltura.srw
forward
global type w_maed_cambioaltura from w_mant_encab_deta_csd
end type
type ids_palletencabhisto from datawindow within w_maed_cambioaltura
end type
type ids_palletfrutahisto from datawindow within w_maed_cambioaltura
end type
end forward

global type w_maed_cambioaltura from w_mant_encab_deta_csd
integer width = 3890
integer height = 1988
string title = "CAMBIO PRODUCTOR / CALIBRE"
string menuname = ""
event ue_imprimir ( )
ids_palletencabhisto ids_palletencabhisto
ids_palletfrutahisto ids_palletfrutahisto
end type
global w_maed_cambioaltura w_maed_cambioaltura

type variables
w_mant_deta_recfruprocee iw_mantencion

DataWindowChild	dw_puerto, dw_planta, dw_ptaori
Integer 				ii_recepcion
Boolean				ib_existe_folio, ib_primera_entrada
String   				is_nombre
end variables

forward prototypes
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean existefolio (string as_columna, string as_valor)
public subroutine habilitaingreso (string columna)
public subroutine habilitaencab (boolean habilita)
public function long buscanuevofolio (integer cliente, integer planta)
public function boolean noexistecliente (integer ai_codigo)
public function string buscacliente (integer cliente)
public function boolean noexisteplanta (string columna)
public function string buscaplanta (integer cliente, integer planta)
public subroutine borra_tablas (integer ai_borra)
public subroutine borra_historico (integer ai_dato)
public function boolean elimina_historico (integer ai_cliente, integer ai_planta, long al_pallet, integer ai_altura)
public function boolean existecambiohis (integer ai_codigo)
end prototypes

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
Integer	li_cliente, li_planta
String	ls_Fecha, ls_obser, ls_numero, ls_cliente, ls_planta
str_info	lstr_info

lstr_info.titulo	= "CAMBIOS DE ALTURA/PRODUCTOR/CALIBRE"
lstr_info.copias	= 1

ls_fecha		=	String(dw_2.Object.altu_fecmov[1])
ls_obser		=	dw_2.Object.altu_observ[1]
ls_numero	=	String(dw_2.Object.altu_numero[1],'00000000')

li_cliente	=	dw_2.Object.clie_codigo[1]
li_planta	=	dw_2.Object.plde_codigo[1]

ls_cliente	= String(li_cliente,'###')+" "+BuscaCliente(li_cliente)
ls_planta	= String(li_planta,'####')+" "+	BuscaPlanta(li_cliente, li_Planta)

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_cambioaltura"

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[1]), Long(istr_mant.argumento[2]),Integer(istr_mant.argumento[3]))

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)

	vinf.dw_1.Modify("Fecha.text = '" + ls_fecha + "'")
	vinf.dw_1.Modify("Observaciones.text = '" + ls_obser + "'")
	vinf.dw_1.Modify("nroguia.text = '" + ls_numero + "'")
	
	IF isnull(is_nombre) OR is_nombre = '' THEN
		IF NOT isnull(dw_2.Object.cahi_codigo[1]) OR dw_2.Object.cahi_codigo[1] <> 0 THEN
			existecambiohis(dw_2.Object.cahi_codigo[1])
		END IF	
	END IF	
	vinf.dw_1.Modify("historico.text = '" + is_nombre + "'")
	vinf.dw_1.Modify("cliente.text = '" + ls_cliente + "'")
	vinf.dw_1.Modify("planta.text = '" + ls_planta + "'")
	
	vinf.Visible	= True
	vinf.Enabled	= True	
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF
SetPointer(Arrow!)
end event

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False
IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF ids_palletfrutahisto.Update(True, False) = 1 THEN
		IF ids_palletencabhisto.Update(True, False) = 1 THEN
			Commit;
				
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, This.Title)	
				RollBack;
			ELSE
				lb_Retorno	=	True
						
				ids_palletfrutahisto.ResetUpdate()
				ids_palletencabhisto.ResetUpdate()
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			RollBack;
		END IF
	ELSE
		 F_ErrorBaseDatos(sqlca, This.Title)
		RollBack;
	END IF
ELSE
	IF dw_2.Update(True, False) = 1 THEN
		IF dw_1.Update(True, False) = 1 THEN
			Commit;
					
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, This.Title)
			
				RollBack;
			ELSE
				lb_Retorno	=	True
			
				dw_2.ResetUpdate()
				dw_1.ResetUpdate()
			END IF
   		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			RollBack;
	  	END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
		RollBack;
	END IF
END IF
		
sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public function boolean existefolio (string as_columna, string as_valor);Integer	li_planta, li_existe,li_cliente, li_tipoen
Long		ll_nfolio

li_planta	=	dw_2.Object.plde_codigo[1]
ll_nfolio 	=	dw_2.Object.altu_numero[1]
li_cliente	=  dw_2.Object.clie_codigo[1]

CHOOSE CASE as_columna
	CASE "plde_codigo"
		li_planta	=	Integer(as_valor)
		
	CASE "altu_numero"
		ll_nfolio 	=	Long(as_valor)
		
	CASE "clie_codigo"
		li_cliente 	=	Integer(as_valor)	
		
END CHOOSE

SELECT  	plde_codigo
	INTO	:li_tipoen
	FROM	dbo.alpalletencab
	WHERE	plde_codigo	=	:li_planta
	AND	altu_numero	=	:ll_nfolio
	AND   clie_codigo =  :li_cliente ;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Alpalletencab")
	RETURN False
ELSEIF sqlca.SQLCode = 0 THEN
	    istr_mant.argumento[1]	= String(li_planta)
	    istr_mant.argumento[2]	= String(ll_nfolio)
       istr_mant.argumento[3]	= String(li_cliente) 
	
	    dw_2.SetItem(1, "clie_codigo",li_cliente)
	    dw_2.SetItem(1, "plde_codigo",li_planta)
	    This.TriggerEvent("ue_recuperadatos")
		 ib_existe_folio	=	True
	    RETURN False
	ELSE
	    IF IsNull(ll_nfolio) THEN
   		 istr_mant.argumento[1]	= String(li_planta)
		    istr_mant.argumento[2]	= String(ll_nfolio)
		    istr_mant.argumento[3]	= String(li_cliente)
			 ib_existe_folio	=	False
		    RETURN False
	    ELSE
		    MessageBox("Atención","Número de Movimiento No ha sido generado. Ingrese Otro.")
 			 ib_existe_folio	=	False
		    RETURN True
	    END IF
    END IF

end function

public subroutine habilitaingreso (string columna);Date	ld_fecha
Integer  li_tarjas, li_tardef
Boolean	lb_estado = True

dw_2.AcceptText()

IF dw_2.RowCount() > 0 THEN
	IF IsNull(dw_2.Object.plde_codigo[1]) OR dw_2.Object.plde_codigo[1] = 0 OR &
		IsNull(dw_2.Object.clie_codigo[1]) OR dw_2.Object.clie_codigo[1] = 0 OR &
		IsNull(dw_2.Object.altu_fecmov[1]) OR dw_2.Object.altu_fecmov[1] = ld_fecha THEN
		lb_estado = False
	END IF
END IF

pb_ins_det.Enabled = lb_estado
end subroutine

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.clie_codigo.Protect	=	0
	dw_2.Object.altu_numero.Protect	=	0
	dw_2.Object.plde_codigo.Protect	=	0
	dw_2.Object.clie_codigo.BackGround.Color 		= RGB(255,255,255)
	dw_2.Object.altu_numero.BackGround.Color	= RGB(255,255,255)
	dw_2.Object.plde_codigo.BackGround.Color		= RGB(255,255,255)
	dw_2.Object.clie_codigo.Color 		= 0
	dw_2.Object.altu_numero.Color	= 0
	dw_2.Object.plde_codigo.Color 	= 0
	
	dw_2.SetColumn("altu_numero")
	dw_2.SetFocus()
ELSE
	dw_2.Object.clie_codigo.Protect	=	1
	dw_2.Object.altu_numero.Protect	=	1
	dw_2.Object.plde_codigo.Protect	=	1
	
	dw_2.Object.clie_codigo.BackGround.Color		= 553648127
	dw_2.Object.altu_numero.BackGround.Color	= 553648127
	dw_2.Object.plde_codigo.BackGround.Color		= 553648127
	dw_2.Object.clie_codigo.Color		=  RGB(255,255,255)
	dw_2.Object.altu_numero.Color	=  RGB(255,255,255)
	dw_2.Object.plde_codigo.Color		=  RGB(255,255,255)

END IF
end subroutine

public function long buscanuevofolio (integer cliente, integer planta);Integer	li_planta, li_cliente
Long		ll_numero

li_cliente	=	cliente	
li_planta	=	planta

SELECT max(altu_numero) INTO:ll_numero
FROM dbo.Alpalletencab
WHERE	clie_codigo = :li_cliente
AND	plde_codigo = :li_planta
AND	altu_numero < 99999999 ;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Alpalletencab")
ELSEIF IsNull(ll_numero) THEN
	ll_numero=1
ELSEIF ll_numero=0 THEN
	ll_numero=1
ELSE
	ll_numero++		
END IF

RETURN ll_numero
end function

public function boolean noexistecliente (integer ai_codigo);String	ls_nombre

SELECT	clie_nombre
	INTO	:ls_nombre  
   FROM	dbo.clientesprod
   WHERE	clie_codigo =	:ai_codigo;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Clientes Producción")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Error", "Cliente no Existe. Ingrese otro.")
	RETURN True
END IF

IF F_ValidaCliente(ai_codigo) THEN
	dw_2.GetChild("plde_codigo", dw_planta)
	dw_planta.SetTransObject(sqlca)
	istr_mant.Argumento[3]	=	String(ai_codigo)
	dw_planta.Retrieve(1)
	RETURN False
ELSE
	RETURN True
END IF

end function

public function string buscacliente (integer cliente);String	ls_Cliente

SELECT clie_nombre
INTO :ls_Cliente
FROM dbo.clientesprod
WHERE clie_codigo = :cliente;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Clientes Producción")
	RETURN "No Existe Cliente"
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Error", "Cliente no Existe. Ingrese otro.")
	RETURN "No Existe Cliente"
	
END IF


RETURN ls_cliente
end function

public function boolean noexisteplanta (string columna);Integer	li_cliente, li_planta, li_tipo

li_cliente	=	Integer(istr_mant.argumento[3])
li_planta	=	Integer(columna)
li_tipo		=	Integer(istr_mant.argumento[13])

SELECT	plde_codigo
	INTO	:li_planta 
   FROM	dbo.plantadesp  
   WHERE plde_codigo	=	:li_planta
	AND	plde_tipopl	=	:li_tipo ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Tabla PLANTADESP")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Codigo Planta/Packing no Existe. Ingrese otro")
	RETURN True
END IF

istr_mant.argumento[15]	=	String(li_planta)

RETURN False
end function

public function string buscaplanta (integer cliente, integer planta);String	ls_planta

SELECT	plde_nombre
	INTO	:ls_planta 
   FROM	dbo.plantadesp  
   WHERE plde_codigo	=	:planta
	AND	plde_tipopl	=	1 ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Tabla PLANTADESP")
	RETURN "No Existe Planta"
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Codigo Planta no Existe. Ingrese otro")
	RETURN "No Existe Planta"
END IF

RETURN ls_planta
end function

public subroutine borra_tablas (integer ai_borra);
end subroutine

public subroutine borra_historico (integer ai_dato);
end subroutine

public function boolean elimina_historico (integer ai_cliente, integer ai_planta, long al_pallet, integer ai_altura);Long  ll_numero

	SELECT count(*)
		INTO   :ll_numero
		FROM   dbo.alpalletfruta
		WHERE  paen_numero =  :al_pallet
		AND    altu_numero <> :ai_altura;

	IF sqlca.SQLCode = -1 THEN
			F_ErrorBaseDatos(sqlca, "Lectura AlPalletFruta")
		RETURN False
	END IF
		
	IF ll_numero > 0 THEN
		MessageBox("ERROR","NO SE PUEDE ELIMINAR EL REGISTRO,~r" + &
				  "~n~nEL PALLET ESTA ASIGNADO A OTRO Nº DE PROCESO")
		RETURN FALSE
	ELSE
		ids_palletencabhisto.Retrieve(ai_cliente,ai_planta,al_pallet)
		ids_palletfrutahisto.Retrieve(ai_cliente,ai_planta,al_pallet)
				
		IF dw_1.DeleteRow(0) = 1 THEN
			ib_borrar = False
			w_main.SetMicroHelp("Borrando Registro...")
			SetPointer(Arrow!)
		ELSE
			ib_borrar = False
			MessageBox(This.Title,"No se puede borrar actual registro.")
		END IF
	
	END IF
RETURN True
end function

public function boolean existecambiohis (integer ai_codigo);String	ls_nombre

SELECT	cahi_nombre
	INTO	:is_nombre  
   FROM	dbo.cambiohistorico
   WHERE	cahi_codigo =	:ai_codigo;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura tabla cambiohistorico")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Error", "cambiohistorico no Existe. Ingrese otro.")
	RETURN True
END IF

RETURN False


end function

event open;//	Argumentos Mantenedor
//	istr_mant.argumento[1]		= 	Código de Planta
//	istr_mant.argumento[2]		= 	Número de Folio Proceso
//	istr_mant.argumento[3]		= 	Código de Exportador
// istr_mant.argumento[8]  	= 	Observaciones
// istr_mant.argumento[9]  	= 	Fecha de Proceso

IF gi_CodExport = 300 THEN dw_2.Object.plde_codigo.dddw.Name	=	"dw_mues_plantadesp"

dw_2.GetChild("plde_codigo", dw_planta)
dw_planta.SetTransObject(sqlca)

dw_planta.Retrieve(1)

x				= 0
y				= 0
This.Height	= 2520
im_menu		= m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.dw				= dw_1
istr_mant.solo_consulta = False

buscar	= "Código:Nvari_codigo,Descripción:Svari_nombre"
ordenar	= "Código:vari_codigo,Descripción:vari_nombre"

ids_palletencabhisto.SetTransObject(sqlca)
ids_palletfrutahisto.SetTransObject(sqlca)

istr_mant.argumento[1]	=	String(gi_codplanta)
istr_mant.argumento[3]	=	String(gi_codexport)
istr_mant.argumento[5]	=	'dw_mues_alpalletfruta'
istr_mant.argumento[9]	=	String(Today())

pb_nuevo.PostEvent(Clicked!)
ib_primera_entrada = True
gb_Repalletizado = False

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)
end event

event ue_borra_detalle;call super::ue_borra_detalle;Long	pallet
Integer li_Altura, li_cliente, li_especie, li_planta

pallet=dw_1.getitemNumber(dw_1.GetRow(),"paen_numero")
li_Altura = dw_2.Object.altu_numero[1]
li_cliente = dw_2.Object.clie_codigo[1]
li_planta = dw_2.Object.plde_codigo[1]

elimina_historico(li_cliente,li_planta,pallet,li_Altura)

IF dw_1.rowcount() < 1 THEN
	dw_1.SetFocus()
	RETURN
END IF

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

THIS.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra	= True
istr_mant.agrega	= False

//OpenWithParm(iw_mantencion, istr_mant)

//istr_mant 				= 	Message.PowerObjectParm
istr_mant.respuesta 	= 	1

IF istr_mant.respuesta = 1 THEN
//	IF dw_1.DeleteRow(0) = 1 THEN
//		ib_borrar = False
//		w_main.SetMicroHelp("Borrando Registro...")
//		SetPointer(Arrow!)
//	ELSE
//		ib_borrar = False
//		MessageBox(This.Title,"No se puede borrar actual registro.")
//	END IF

	IF dw_1.RowCount() = 0 THEN 
		HabilitaEncab(True)
		pb_eli_det.Enabled = False
	END IF
END IF

istr_mant.borra	 = False
end event

event ue_nuevo_detalle;Long		il_tarjas, il_tardef

istr_mant.agrega	=	True
istr_mant.argumento[45] =  String(dw_2.Object.altu_numero[1])
istr_mant.argumento[46] = String(3)
istr_mant.argumento[50] = String(1)

OpenWithParm(w_maed_palletencabhistoria_cambioprodcal, istr_mant)

IF dw_1.RowCount() > 0 THEN 
	
	HabilitaEncab(False)
	pb_eliminar.Enabled	= 	FALSE
	pb_buscar.Enabled		=	FALSE
	pb_nuevo.Enabled		=	FALSE
	
	pb_grabar.Enabled		= 	TRUE

	dw_1.SetRow(il_fila)
	dw_1.SelectRow(il_fila,True)

END IF


end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta, pallet

DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	ll_fila_e	= dw_2.Retrieve(Integer(istr_mant.argumento[1]), Long(istr_mant.argumento[2]),Integer(istr_mant.argumento[3]))
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		DO
			ll_fila_d	= dw_1.Retrieve(Integer(istr_mant.argumento[1]), Long(istr_mant.argumento[2]),Integer(istr_mant.argumento[3]))

			IF ll_fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			
			ELSE                                                    
				pb_eliminar.Enabled	= True
				pb_grabar.Enabled		= True
				pb_imprimir.Enabled	= True
				pb_ins_det.Enabled	= True

				IF ll_fila_d > 0 THEN
					pb_eli_det.Enabled	= True
					dw_1.SetRow(1)
					dw_1.SelectRow(1,True)
					dw_1.SetFocus()
					HabilitaEncab(False)
					
	
   			ELSE
//	   		pb_ins_det.SetFocus()
				END IF
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

on w_maed_cambioaltura.create
int iCurrent
call super::create
this.ids_palletencabhisto=create ids_palletencabhisto
this.ids_palletfrutahisto=create ids_palletfrutahisto
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.ids_palletencabhisto
this.Control[iCurrent+2]=this.ids_palletfrutahisto
end on

on w_maed_cambioaltura.destroy
call super::destroy
destroy(this.ids_palletencabhisto)
destroy(this.ids_palletfrutahisto)
end on

event ue_nuevo();HabilitaEncab(True)

ib_ok	= True

CHOOSE CASE wf_modifica()
	CASE -1
		ib_ok = False
	CASE 1
		Message.DoubleParm = 0
		This.TriggerEvent("ue_guardar")
		IF message.DoubleParm = -1 THEN ib_ok = False
	CASE 3
		ib_ok	= False
		RETURN
END CHOOSE

IF ib_ok = False THEN RETURN

dw_1.Reset()

pb_eli_det.Enabled		= False
pb_ins_det.Enabled		= False
pb_grabar.Enabled			= False
pb_eliminar.Enabled		= False
pb_imprimir.Enabled		= False
istr_mant.solo_consulta	= False
dw_2.Enabled				= True

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetItem(1, "clie_codigo", gi_codexport)
dw_2.SetItem(1, "plde_codigo", gi_codplanta)
dw_2.SetRedraw(True)

dw_2.SetFocus()
end event

event ue_seleccion;call super::ue_seleccion;istr_busq.argum[1]	= istr_mant.argumento[3]	
istr_busq.argum[2]	= istr_mant.argumento[1]	
istr_busq.argum[3]	= '6'

OpenWithParm(w_busc_cambioaltura, istr_busq)

istr_busq	       = Message.PowerObjectParm

IF istr_busq.argum[5] <> "" THEN
	istr_mant.argumento[1]	= istr_busq.argum[2]
	istr_mant.argumento[2]	= istr_busq.argum[5]
	istr_mant.argumento[3]	= istr_busq.argum[1]
	ib_existe_folio	=	True
	This.TriggerEvent("ue_recuperadatos")
ELSE
	pb_buscar.SetFocus()
END IF

end event

event ue_modifica_detalle;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False
	
	istr_mant.argumento[7] = ""
	istr_mant.argumento[6] = String(dw_1.GetitemNumber(dw_1.GetRow(),"paen_numero"))
		
	OpenWithParm(w_maed_palletencabhistoria_cambioprodcal, istr_mant)
		
END IF
end event

event ue_antesguardar;call super::ue_antesguardar;Long 		ll_nuevofolio
Integer	li_fillas

/*
Envia los datos al bufer de eliminación
*/
ids_Palletencabhisto.RowsMove(1,ids_Palletencabhisto.Rowcount(),Primary!,ids_Palletencabhisto,1,Delete!)	
ids_Palletfrutahisto.RowsMove(1,ids_Palletfrutahisto.Rowcount(),Primary!,ids_Palletfrutahisto,1,Delete!)

IF dw_2.Object.altu_numero[1]<= 0 THEN
	RETURN
ELSE
	IF Not ib_primera_entrada AND Not ib_existe_folio  THEN
		/*
		Se actualiza tabla Alpalletencab a objeto de bloquearla hasta que termine la grabación
		del ingreso
		*/
//		UPDATE dba.Alpalletencab
//			SET altu_numero = 0
//			WHERE 1=2;
	
		ll_nuevofolio=BuscaNuevoFolio(Integer(istr_mant.argumento[3]),Integer(istr_mant.argumento[1]))
		
		dw_2.Object.altu_numero[1]	= ll_nuevofolio
		dw_2.SetItem(1, "altu_numero",ll_nuevofolio)
	
		istr_mant.argumento[2]	= String(ll_nuevofolio)
		
		FOR li_fillas = 1 TO dw_1.RowCount()
			 dw_1.Object.altu_numero[li_fillas]	= ll_nuevofolio
		NEXT
	END IF
END IF
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF  wf_actualiza_db(True)  THEN
	//lb_borrado	=	True
END IF

IF wf_actualiza_db(False) THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_eliminar.Enabled	= True
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF

pb_eliminar.Enabled	= 	TRUE
pb_buscar.Enabled	=	TRUE
pb_nuevo.Enabled		=	TRUE


end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_cambioaltura
integer x = 105
integer y = 636
integer width = 3026
integer height = 1124
integer taborder = 100
string title = "Detalle de Pallets"
string dataobject = "dw_mues_alpalletfruta"
boolean livescroll = false
end type

event dw_1::dragdrop;call super::dragdrop;dw_1.Object.objetname.Moveable = 0 
end event

event dw_1::doubleclicked;//
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_cambioaltura
integer x = 119
integer y = 36
integer width = 2898
integer height = 564
string dataobject = "dw_mant_alpalletencab"
end type

event dw_2::itemchanged;call super::itemchanged;String	ls_columna, ls_nula
Date		ld_nula
DataWIndowChild	dw_calibres

SetNull(ls_nula)
SetNull(ld_nula)

ls_columna = GetColumnName()

CHOOSE CASE ls_columna
	CASE "plde_codigo"
		IF ExisteFolio(ls_columna, data) THEN
			This.SetItem(1, ls_columna, Integer(ls_nula))
			RETURN 1
		END IF
		
	CASE "altu_numero"
		IF ExisteFolio(ls_columna, data) THEN
			This.SetItem(1, ls_columna, Integer(ls_nula))
			RETURN 1
		END IF
		
	CASE "clie_codigo"
		IF NoExisteCliente(Integer(data)) THEN
			This.SetItem(Row, ls_Columna, gi_codexport)
			RETURN 1
		END IF	
	
	CASE "altu_fecmov"
		IF Not f_validafechatempo(date(data)) THEN
			This.SetItem(Row, ls_Columna, ld_nula)
			RETURN 1
		END IF		

		istr_mant.argumento[9]	= data
		
	CASE "cahi_codigo"
		IF existecambiohis(Integer(data)) THEN
			This.SetItem(Row, ls_Columna, Integer(ls_nula))
			RETURN 1
		END IF			
		
END CHOOSE

HabilitaIngreso(ls_columna)
end event

event dw_2::doubleclicked;//
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_cambioaltura
integer x = 3538
integer y = 304
end type

event pb_nuevo::clicked;call super::clicked;ib_primera_entrada = True
ib_existe_folio	 =	False

call super:: clicked
end event

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_cambioaltura
integer x = 3538
integer y = 484
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_cambioaltura
integer x = 3538
integer y = 664
end type

event pb_grabar::clicked;ib_primera_entrada = False

call super:: clicked

ib_existe_folio = True
end event

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_cambioaltura
integer x = 3538
integer y = 840
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_cambioaltura
integer x = 3538
integer y = 1024
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_cambioaltura
integer x = 3538
integer y = 1328
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_cambioaltura
integer x = 3538
integer y = 1564
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_cambioaltura
integer x = 3534
integer y = 124
end type

type ids_palletencabhisto from datawindow within w_maed_cambioaltura
boolean visible = false
integer x = 3122
integer y = 124
integer width = 325
integer height = 212
integer taborder = 70
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_palletencabhisto_ds"
borderstyle borderstyle = stylelowered!
end type

type ids_palletfrutahisto from datawindow within w_maed_cambioaltura
boolean visible = false
integer x = 3122
integer y = 336
integer width = 320
integer height = 236
integer taborder = 100
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_palletfrutahisto_ds"
boolean minbox = true
borderstyle borderstyle = stylelowered!
end type

