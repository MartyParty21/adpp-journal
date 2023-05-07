package cz.agents.alite.communication;

import cz.agents.alite.communication.content.Content;

import java.util.ArrayList;
import java.util.List;

/**
 * Communicator set to delegate all the work regarding communication channel and message passing to external listener
 */
public class ExternalCommunicator implements Communicator {
    private final MessageHandler sender;

    public ExternalCommunicator(MessageHandler sender) {
        this.sender = sender;
    }

    @Override
    public Message createMessage(Content content) {
        return null;
    }

    @Override
    public Message createReply(Message message, Content content) {
        return null;
    }

    @Override
    public void addMessageHandler(MessageHandler messageHandler) {

    }

    @Override
    public void removeMessageHandler(MessageHandler messageHandler) {

    }

    @Override
    public void receiveMessage(Message message) {

    }

    @Override
    public void sendMessage(Message message) {

    }

    @Override
    public String getAddress() {
        return null;
    }
}
